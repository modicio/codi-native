/**
 * Copyright 2022 Karl Kegel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package codi.core

import codi.core.datamappings.{AssociationData, AttributeData, ExtensionData, InstanceData}
import codi.core.rules.AttributeRule

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * <p> The DeepInstance is the central class of the clabject-hierarchy. See the external documentation for more information
 * and examples.
 * <p> A DeepInstance must have a [[codi.core.TypeHandle TypeHandle]] containing its instantiated type-fragment;
 * A [[codi.core.Registry Registry]] for lazy reloads on unfold() calls, which contains the all instances and fragments
 * used in the sam runtime-environment.
 * <p> A DeepInstance is identified by a technical unique instanceId and an identity representing the clabject.
 * <p> <strong>Following the intended usage, DeepInstances are always constructed using the [[codi.core.InstanceFactory InstanceFactory]]</strong>
 * <p> Objectified members of a DeepInstance are bundled by its [[codi.core.Shape Shape]].
 * <p> A DeepInstance serialises to the data-tuple represented by the [[codi.core.datamappings.InstanceData InstanceData]] case-class.
 * <br />
 * <br />
 * <p> A DeepInstance is a foldable entity. If the DeepInstance is loaded from the Registry or constructed by the InstanceFactory,
 * their extensions are only available as [[codi.core.datamappings.ExtensionData ExtensionData]] part of the Shape.
 * <strong>The client must call unfold() if deep queries and operations should be performed.</strong> Only by calling unfold(),
 * the extension hierarchy is loaded from the Registry and wired in form of unfoldedExtensions.
 *
 * @see [[codi.core.InstanceFactory]]<p>[[codi.core.datamappings.InstanceData]]<p>[[codi.core.Shape]]
 * @param instanceId unique technical identifier, must be specified during deep-instantiation
 * @param identity   unique identity of the overall clabject
 * @param shape      contains all objectified values such as [[codi.core.datamappings.AssociationData AssociationData]],
 *                   [[codi.core.datamappings.ExtensionData ExtensionData]] and [[codi.core.datamappings.AttributeData AttributeData]]
 * @param typeHandle the instantiated [[codi.core.Fragment Fragment]] type, represented by its [[codi.core.TypeHandle TypeHandle]]
 * @param registry   the [[codi.core.Registry Registry]] of the runtime-environment
 */
class DeepInstance(instanceId: String, identity: String, shape: Shape, typeHandle: TypeHandle, registry: Registry) {

  /**
   * <p>Contains the concrete extensions after unfold() was called.
   */
  private val unfoldedExtensions: mutable.Set[DeepInstance] = mutable.Set[DeepInstance]()

  /**
   * <p> Produces the [[codi.core.TypeHandle TypeHandle]] of the associated [[codi.core.Fragment Fragment]] type.
   * <p> If the DeepInstance is not unfolded, the Fragment is not unfolded as well!
   *
   * @return TypeHandle
   */
  def getTypeHandle: TypeHandle = typeHandle

  /**
   * <p> Get the instanceId serving as the technical unique identifier.
   *
   * @return String - instanceId
   */
  def getInstanceId: String = instanceId

  /**
   * <p> Unfold the given DeepInstance asynchronously. This operation loads all DeepInstances specified by
   * [[codi.core.datamappings.ExtensionData ExtensionData]] from the provided [[codi.core.Registry Registry]] and adds
   * them to the concreteExtensions Set. The unfold call is propagated to the loaded extensions as well. The unfold call
   * is also propagated to the [[codi.core.Fragment Fragment]] calling [[codi.core.Fragment#unfold Fragment.unfold()]]
   * <p> <strong>This method must be called before deep queries and operations such as deepAttributeMap() and similar methods
   * starting with deep...() are used. They will produce local-only results otherwise.</strong>
   * <p> If an already unfolded DeepInstance is unfolded again, the wired extensions are cleared and this method recreates
   * the hierarchy from scratch. Consequently, a client should avoid unnecessary repeated unfold() calls.
   * <p> <strong>This operation MUTATES the called DeepInstance and returns the this-pointer afterwards. Usage of the return value
   * is not required!</strong>
   *
   * @return Future[DeepInstances] - the unfolded DeepInstance after completion (this-pointer of mutated instance)
   */
  def unfold(): Future[DeepInstance] = {
    typeHandle.unfold() flatMap (_ => {
      Future.sequence(shape.getExtensions.map(_.parentInstanceId).map(registry.get)) flatMap (extensionOptions => {
        unfoldedExtensions.clear()
        unfoldedExtensions.addAll(extensionOptions.filter(_.isDefined).map(_.get))
        Future.sequence(unfoldedExtensions.map(_.unfold())) map (_ => this)
      })
    })
  }

  /**
   * <p> Verifies the DeepInstance and returns the result. This Operation checks the structure for conflicts and
   * propagates the verify call to its members [[codi.core.Shape Shape]] and [[codi.core.TypeHandle TypeHandle]]
   * <p> This operation makes use of the provided [[codi.verification.ModelVerifier ModelVerifier]] and
   * [[codi.verification.DefinitionVerifier DefinitionVerifier]]. Different Verifier implementations can lead to different
   * results on the same DeepInstance.
   * <p> TODO not implemented yet, returns always true
   *
   * @return
   */
  def verify(): Boolean = {
    true
  }

  /**
   * <p> Store the current state of the DeepInstance in the [[codi.core.Registry Registry]].
   * <p> <strong>To store the complete DeepInstance, it needs to be unfolded.</strong> If not, only the called hierarchy
   * node is stored.
   * <p> The calling client should await the returned Future to avoid race-conditions.
   *
   * @return Future[Any] - asynchronous response after storing the DeepInstance
   */
  def commit: Future[Any] = {
    registry.setInstance(this) flatMap (_ => Future.sequence(unfoldedExtensions.map(_.commit)))
  }

  /**
   * <p> Instance-tree operation to extract the largest higher-level subtree which is typeOf the specified typeName.
   * If the called DeepInstance is of the given typeName, it is returned directly.
   * <p> If no subtree with the given type is found, i.e. the typeName is not part of
   * [[codi.core.DeepInstance#getTypeClosure DeepInstance.getTypeClosure()]], an empty Option is returned.
   * <p> <strong>This operation requires the DeepInstance to be unfolded!</strong>
   *
   * @param typeName name of the type, which subtree of the instance-hierarchy should be returned
   * @return Option[DeepInstance] - unfolded DeepInstance tree of the requested type
   */
  def getPolymorphSubtype(typeName: String): Option[DeepInstance] = {
    if (typeName == typeHandle.fragment.name) {
      return Some(this)
    }
    if (getTypeClosure.contains(typeName)) {
      unfoldedExtensions.map(_.getPolymorphSubtype(typeName)).filter(_.isDefined).head
    } else {
      None
    }
  }

  /**
   * <p> Produce the data representation of this DeepInstance and its [[codi.core.Shape Shape]].
   * <p> Following the default usage, this operation is used internally only.
   * <p> <strong>This operation requires the DeepInstance to be unfolded!</strong>
   *
   * @return Tuple of data representations
   */
  def toData: (InstanceData, Set[ExtensionData], Set[AttributeData], Set[AssociationData]) = {
    (
      InstanceData(instanceId, typeHandle.getTypeName, identity),
      shape.getExtensions,
      shape.getAttributes,
      shape.getAssociations
    )
  }

  /**
   * <p> Get the map of concrete [[codi.core.datamappings.AttributeData AttributeData]] together with their respective
   * [[codi.core.rules.AttributeRule AttributeRules]].
   * <p> This operations returns attributes of the current hierarchy node only.
   * <p> Duplicate attribute keys are resolved by sorting attributes by their technical id and taking the alphanumerical
   * biggest id only. However, duplicates should be avoided on model-side.
   *
   * @return Map[AttributeData, AttributeRule] - attributes of this particular DeepInstance
   */
  def attributeMap(): Map[AttributeData, AttributeRule] = {
    val propertyRules = typeHandle.fragment.definition.getAttributeRules
    shape.getAttributes.toSeq.sortBy(_.id).map(property => (property, propertyRules.find(_.name == property.key).get)).toMap
  }

  /**
   * <p> Get the deep map of concrete [[codi.core.datamappings.AttributeData AttributeData]] together with their respective
   * [[codi.core.rules.AttributeRule AttributeRules]].
   * <p> Duplicate attribute keys on the same level are handled by [[codi.core.DeepInstance#attributeMap DeepInstance.attributeMap()]].
   * Duplicate keys of higher levels are resolved as follows: Extensions are sorted by their typeName alphabetically. Out of
   * those, the attribute of the alphabetical prior extension is taken. In between levels, attributes of children override
   * parent specifications ignoring the datatype.
   * <p> <strong>This operation requires the DeepInstance to be unfolded!</strong>
   *
   * @return Map[AttributeData, AttributeRule] - deep attribute map with resolved duplicates and overrides
   */
  def deepAttributeMap(): Map[AttributeData, AttributeRule] = {
    val baseMap: mutable.Map[AttributeData, AttributeRule] = mutable.Map.from(attributeMap())
    unfoldedExtensions.toSeq.sortBy(_.getTypeHandle.getTypeName).foreach(extension => {

      val extensionPropertyMap = extension.deepAttributeMap()
      extensionPropertyMap.foreach(entry => {

        val (property, _) = entry
        if (!baseMap.exists(_._1.key == property.key)) {
          baseMap.addOne(entry)
        }
      })
    })
    baseMap.toMap
  }

  /**
   * <p> Get the set of type-names which can be associated to this DeepInstance. The results do only contain type-names
   * as specified in the type-model. Polymorph smaller types which are also possible are not returned explicitly. The result
   * does not contain information which type can be used by which particular relation!
   * <p> In the unfolded case, only associations of this particular instance node are returned
   * <p> The DeepInstance has to be unfolded to return a complete result.
   *
   * @return Set[String] - (deep) possible associations
   */
  def associationTypes: Set[String] = {
    unfoldedExtensions.flatMap(_.associationTypes).toSet ++ typeHandle.getAssociated.map(_.getTypeName)
  }

  /**
   * <p> Get all association relation-names which are allowed by this DeepInstance together with the type-names that can
   * be associated by the corresponding relation.
   * <p> This method only collects associations which are allowed by the type of this particular instance-node.
   *
   * @return Map[ String, mutable.Set[String] ] - associationName -> {typeName,...}
   */
  def associationRuleMap: Map[String, mutable.Set[String]] = {
    val results: mutable.Map[String, mutable.Set[String]] = mutable.Map()
    typeHandle.getFragment.definition.getAssociationRules.foreach(rule => {
      val key = rule.associationName
      val value = rule.targetName
      val entry = results.get(key)
      if (entry.nonEmpty) {
        entry.get.add(value)
      } else {
        results.addOne((key, mutable.Set(value)))
      }
    })
    results.toMap
  }

  /**
   * <p> Get all association relation-names which are allowed the DeepInstance hierarchy together with the type-names that can
   * be associated by the corresponding relation.
   * <p> <strong>This operation requires the DeepInstance to be unfolded!</strong>
   *
   * @return Map[ String, mutable.Set[String] ] - associationName -> {typeName,...}
   */
  def deepAssociationRuleMap: Map[String, mutable.Set[String]] = {
    val results: mutable.Map[String, mutable.Set[String]] = mutable.Map.from(associationRuleMap)
    unfoldedExtensions.foreach(parent => {
      val parentResults = parent.deepAssociationRuleMap
      parentResults.foreach(res => {
        val (key, values) = res
        val entry = results.get(key)
        if (entry.nonEmpty) {
          entry.get.addAll(values)
        } else {
          results.addOne((key, values))
        }
      })
    })
    results.toMap
  }

  /**
   * <p> Change the value of an attribute of the local DeepInstance node.
   * <p> The value is checked against the type, which may lead to a controlled exception.
   * <p> If the attribute key is not found in this instance, false is returned.
   * <p> <strong>This operation is not persistent. The client has to call [[codi.core.DeepInstance#commit DeepInstance.commit()]]
   * manually.</strong>
   *
   * @param key   attribute to change the value
   * @param value new value
   * @return Boolean - false if attribute not found
   */
  def assignValue(key: String, value: String): Boolean = {
    val attributeOption = shape.getAttribute(key)
    if (attributeOption.isDefined) {
      attributeOption.get.value = value
      true
    } else {
      false
    }
  }

  /**
   * <p> Change the value of an attribute of the DeepInstance.
   * <p> The value is checked against the type, which may lead to a controlled exception.
   * <p> If the attribute key is not found in this instance, the call is propagated to all parent instances.
   * <p> <strong>This operation is not persistent. The client has to call [[codi.core.DeepInstance#commit DeepInstance.commit()]]
   * manually.</strong>
   * <p> <strong>This operation requires the DeepInstance to be unfolded!</strong>
   *
   * @param key   attribute to change the value
   * @param value new value
   * @return Boolean - false if attribute not found
   */
  def assignDeepValue(key: String, value: String): Boolean = {
    val attributeOption = deepAttributeMap().keySet.find(_.key == key)
    if (attributeOption.isDefined) {
      attributeOption.get.value = value
      true
    } else {
      false
    }
  }

  /**
   * <p> Get the value of a local attribute by its key.
   *
   * @param key attribute key
   * @return Option[String] - value of the attribute, None if attribute is not found
   */
  def value(key: String): Option[String] = {
    shape.getAttribute(key).map(_.value)
  }

  /**
   * <p> Get the value of an attribute of the DeepInstance hierarchy by its key.
   * <p> <strong>This operation requires the DeepInstance to be unfolded!</strong>
   *
   * @param key attribute key
   * @return Option[String] - value of the attribute, None if attribute is not found
   */
  def deepValue(key: String): Option[String] = {
    deepAttributeMap().keySet.find(_.key == key).map(_.value)
  }

  /**
   * <p> Get all plain [[codi.core.datamappings.AssociationData AssociationData]] provided by the local
   * [[codi.core.Shape Shape]] of this DeepInstance.
   *
   * @return Set[AssociationData] (deep immutable)
   */
  def getAssociations: Set[AssociationData] = shape.getAssociations

  /**
   * <p> Get all plain [[codi.core.datamappings.AssociationData AssociationData]] provided by the
   * [[codi.core.Shape Shapes]] of this DeepInstance and its extension hierarchy.
   * <p> <strong>This operation requires the DeepInstance to be unfolded!</strong>
   *
   * @return Set[AssociationData] (deep immutable)
   */
  def getDeepAssociations: Set[AssociationData] = shape.getAssociations ++ unfoldedExtensions.flatMap(_.getDeepAssociations)

  /**
   * <p> Remove a concrete [[codi.core.datamappings.AssociationData AssociationData]] from the
   * [[codi.core.Shape Shapes]] of this DeepInstance. If the instance is unfolded, this call is propagated throughout
   * the extension hierarchy.
   *
   * @param associationId id of the [[codi.core.datamappings.AssociationData AssociationData]] to remove
   */
  def removeAssociation(associationId: Long): Unit = {
    shape.removeAssociation(associationId)
    unfoldedExtensions.foreach(_.removeAssociation(associationId))
  }

  /**
   * <p> Get the closure of the types of this DeepInstance, i.e. get the set of type-names of all
   * [[codi.core.Fragment Fragments]] in this instances type-hierarchy.
   * <p> The DeepInstance can be used as any of the types as provided by this method.
   * <p> <strong>This operation requires the DeepInstance (or the TypeHandle only) to be unfolded!</strong>
   *
   * @return Set[String] of all type names the DeepInstance type-hierarchy contains.
   */
  def getTypeClosure: Set[String] = {
    getTypeHandle.getFragment.getTypeClosure
  }

  /**
   * <p>Associate a given DeepInstance to this DeepInstance. Such an association represents a concrete
   * [[codi.core.rules.AssociationRule AssociationRule]] in form of [[codi.core.datamappings.AssociationData AssociationData]].
   * <p> To establish an association, the polymorph type of the provided DeepInstance and the relation name are required.
   * This operation checks if the association is allowed by a corresponding [[codi.core.rules.AssociationRule AssociationRule]] and
   * if the provided associateAs fragment-type is allowed by the rule or by the fuzzy polymorphism.
   *
   * <p> <strong>Fuzzy polymorphism</strong> means that the associate operation checks if the provided DeepInstance has a
   * type in its type-closure that is target to the association rule representing the given byRelation. If this is fulfilled,
   * the association can be established. In the case that associateAs (a type-name) is part of the closure of types of the
   * DeepInstance which is associated AND the associateAs-type subtree is bigger then the subtree of the type required
   * by the relation, the bigger subtree is taken. This can be understood as annotating a cast to the association.
   *
   * <p> <strong>This operation requires the DeepInstance and the provided DeepInstance to be unfolded!</strong>
   *
   * @param deepInstance the [[codi.core.DeepInstance DeepInstance]] to associate by the given relation
   * @param associateAs  the type-name annotated to the association to address the associated DeepInstance. The provided DeepInstance must
   *                     have this type in its type-closure
   * @param byRelation   the relation-name of the [[codi.core.rules.AssociationRule AssociationRule]]
   * @return Boolean - if the operation finished successful
   */
  def associate(deepInstance: DeepInstance, associateAs: String, byRelation: String): Boolean = {
    //check first if the instance has a type in their composite hierarchy that can be associated by any rule
    val instanceTypes: Set[String] = deepInstance.getTypeClosure
    println("Association Candidate: " + instanceTypes)
    println("Association Target: " + getTypeClosure)
    println("byRelation: " + byRelation)
    println("associateAs: " + associateAs)
    if (instanceTypes.contains(associateAs)) {
      //check if the proposed relation is defined on top of a type which is in the instance type hierarchy
      val allowedAssociationTypes = typeHandle.getFragment.getDeepAssociationRulesOfRelation(byRelation).map(_.targetName)
      if (instanceTypes.intersect(allowedAssociationTypes).nonEmpty) {
        shape.addAssociation(AssociationData(0, byRelation, instanceId, deepInstance.getPolymorphSubtype(associateAs).get.getInstanceId, isFinal = false))
        true
      } else {
        throw new Exception("The allowed association types does not contain the given type or the instance does not intersect with the given type")
      }
    } else {
      throw new Exception("The proposed relation is not defined on top of a type which is in the instance type hierarchy")
    }
  }
}

object DeepInstance {

  def deriveSingletonInstanceId(identity: String, fragmentName: String): String = {
    identity + ":" + fragmentName
  }

  def isSingletonInstanceId(instanceId: String): Boolean = {
    instanceId.startsWith("$")
  }

  def isSingletonRoot(instanceIdPrefix: String): Boolean = {
    isSingletonInstanceId(instanceIdPrefix) &&
      instanceIdPrefix.split("_").length > 1 &&
      instanceIdPrefix.split(":").length == 1
  }

  def deriveRootSingletonInstanceId(fragmentName: String): String = {
    Fragment.composeSingletonIdentity(fragmentName) + ":" + fragmentName
  }

}