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

import codi.core.Fragment.composeSingletonIdentity
import codi.core.datamappings.{FragmentData, RuleData}
import codi.core.rules.{AssociationRule, AttributeRule}
import codi.core.values.ConcreteValue
import codi.util.Observer
import codi.verification.{DefinitionVerifier, ModelVerifier}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * <p> The Fragment is the central class of the CoDI type-hierarchy. It forms the component of the type-composite tree.
 * <p> The abstract Fragment contains all concrete functionality to handle associations and rules. Methods regarding extensions
 * are abstract and must be implemented by the [[codi.core.Node Node]]. Other abstract members must be extended or overwritten by
 * its child classes [[codi.core.Node Node]] and [[codi.core.BaseModel BaseModel]]
 * <p> Apart from the constructor arguments, the Fragment possess a [[codi.core.Definition Definition]], a [[codi.core.Registry Registry]], a
 * [[codi.verification.DefinitionVerifier DefinitionVerifier]] and a [[codi.verification.ModelVerifier ModelVerifier]].Those classes are
 * not always required but should be set directly after object construction using their respective set-methods.
 *
 * @param name       name of the Fragment, name and identity form a unique pair.
 * @param identity   identity of the Fragment
 * @param isTemplate if the template can be instantiated directly or only used as part of an extension hierarchy / isAbstract
 */
abstract class Fragment(val name: String, val identity: String, val isTemplate: Boolean) {

  private final var definitionOption: Option[Definition] = None
  private final var registryOption: Option[Registry] = None

  protected final var modelVerifier: Option[ModelVerifier] = None
  protected final var definitionVerifier: Option[DefinitionVerifier] = None

  private[core] val associations: mutable.Set[TypeHandle] = mutable.Set()

  /**
   * <p> Private observer object representing a callback that is called if the [[codi.core.Definition Definition]]
   * changes.
   */
  private final object _definitionObserver extends Observer {
    override def onChange(): Future[Unit] = {
      //TODO commit here if in some kind of automatic mode
      Future.successful()
    }
  }

  /**
   * <p>Abstract method to get if the concrete implementation is a [[codi.core.Node Node]]. It must be a
   * [[codi.core.BaseModel BaseModel]] otherwise.
   *
   * @return Boolean- true if Node
   */
  def isNode: Boolean

  /**
   * <p> Abstract method to get the set of parents i.e. the set of extensions.
   * <p> See the concrete implementations for more information.
   * <p> A concrete implementation may require the Fragment to be unfolded!
   *
   * @return Set[Fragment] - set of parent/extended Fragments
   */
  def getParents: Set[Fragment]

  /**
   * <p> Add a [[codi.core.Rule Rule]] to this Fragment.
   * <p> See the concrete implementations for more information.
   * <p> A concrete implementation may require the Fragment to be unfolded!
   * <p> A concrete implementation may change the fold state of the Fragment
   *
   * @param rule the Rule to add
   */
  def applyRule(rule: Rule): Unit

  /**
   * <p> Remove a [[codi.core.Rule Rule]] from this Fragment.
   * <p> See the concrete implementations for more information.
   * <p> A concrete implementation may require the Fragment to be unfolded!
   * <p> A concrete implementation may change the fold state of the Fragment
   *
   * @param rule the Rule to remove
   */
  def removeRule(rule: Rule): Unit

  /**
   * <p> Abstract framework-private method to produce a data representation of this Fragment.
   * <p> See the concrete implementations for more information.
   * <p> A concrete implementation may require the Fragment to be unfolded!
   *
   * @return (FragmentData, Set[RuleData]) - tuple of [[codi.core.datamappings.FragmentData FragmentData]] and
   *         [[codi.core.datamappings.RuleData RuleData]]
   */
  private[codi] def toData: (FragmentData, Set[RuleData])

  /**
   * <p> Trigger the persistence process for this Fragment. Child classes may overwrite the behaviour of this method.
   * <p> A concrete implementation may require the Fragment to be unfolded!
   * <p> See overwriting implementations for more information.
   *
   * @return Future[Unit] - after the persistence process was completed
   */
  def commit(): Future[Unit] = {
    registry.setType(this.createHandle)
  }

  /**
   * <p> Fork this Fragment. This operation creates a copy of Fragment and [[codi.core.Definition Definition]] with
   * a new identity propagated through the model.
   * <p> Note: this operations does only work for this particular Fragment in general case. If called on a Node,
   * the refined implementation can change this behaviour.
   * <p> The forked Fragment is added to the registry immediately.
   * <p> TODO this operation is lazy, it returns before the Fragment is stored in the registry.
   *      This operation should return a Future instead.
   *
   * <p> <strong>If a BaseModel is forked, it becomes a Node!<strong>
   *
   * @param identity the new identity the forked Fragment receives
   * @return Fragment - the forked Fragment
   */
  def fork(identity: String): Fragment = {
    val newNode = new Node(name, identity, isTemplate)
    newNode.setRegistry(registry)
    newNode.setDefinition(definition.fork(identity))
    registry.setType(newNode.createHandle)
    newNode
  }

  /**
   * <p> Unfolds all associations of the Fragment.
   * <p> This call is not propagated to the associated Fragments, they remain folded!
   * <p> By default, this operation is used by the [[codi.core.TypeHandle TypeHandle]] only.
   * <p> A [[codi.core.Registry Registry]] must be available to this Fragment because not available data is lazily reloaded.
   * <p> Note that only reference fragments are resolved. In an instantiated case (identity != #), the reference specifications must not match with different instance identities so those
   * should always be fetched via the respective InstanceHandle.
   *
   * @return Future[Unit] - after the operation terminates. The Fragment should not be accessed before this operation terminates.
   *         Exception in error cases.
   */
  def unfold(): Future[Any] = {
    associations.clear()
    // resolve associations
    val associationRules: Set[AssociationRule] = definition.getAssociationRules
    if (associationRules.isEmpty) {
      Future.successful()
    } else {
      if (associationRules.nonEmpty && registryOption.isEmpty) {
        throw new Exception("Undefined registry in fragment to unfold")
      }
      val mapOfFutures = associationRules.map(associationRule => registry.getType(associationRule.targetName, Fragment.REFERENCE_IDENTITY))
      val associationsFuture = Future.sequence(mapOfFutures)

      associationsFuture map (handleOptions => {
        //FIXME here we do nothing if a target is not found
        associations.addAll(handleOptions.filter(_.isDefined).map(_.get))
      })
    }
  }

  /**
   * <p> Undo the unfold() operation. This clears the internal set of unfolded associations.
   */
  def fold(): Unit = {
    associations.clear()
  }

  /**
   * FIXME this is a dummy implementation and needs to be moved to a verification or reasoning module in the future.
   *  No verification is done here, if a non-existent association is given, it will be accepted!
   * @return
   */
  def isConcrete: Boolean = {
    //TODO check if every attribute and association with mult 1 has one final value
    //val associationRules = get

    val associationRules = deepAssociationRuleSet
    val attributeRules = deepAttributeRuleSet
    val values = deepValueSet
    val associationValues = values.filter(_.isAssociationValue)
    val attributeValues = values.filter(_.isAttributeValue)

    var isConcrete: Boolean = true

    //each association must have a fixed int-value multiplicity.
    //the number of values fulfilling this association must match the multiplicity
    associationRules.foreach(associationRule => {
      val relation = associationRule.associationName
      if(associationRule.hasIntMultiplicity){
        val multiplicity = associationRule.getIntMultiplicity
        if(associationValues.count(value => value.valueName == relation) != multiplicity){
          isConcrete = false
        }
      }else{
        isConcrete = false
      }
    })

    //each attribute rule must be fulfilled by exactly one value
    attributeRules.foreach(attributeRule => {
      if(attributeValues.count(value => value.valueName == attributeRule.name) != 1){
        isConcrete = false
      }
    })
    println("DEBUG >> " + name + " isConcrete = " + isConcrete)
    isConcrete
  }

  def hasSingleton: Future[Boolean] = {
    registry.getSingletonTypes(name) map(_.nonEmpty)
  }

  def hasSingletonRoot: Future[Boolean] = {
    registry.getType(name, composeSingletonIdentity(name)) map (_.isDefined)
  }

  def updateSingletonRoot(): Future[Option[DeepInstance]] = {
    unfold() flatMap (_ => removeSingleton() flatMap (_ => {
      if (!isConcrete) {
        Future.successful(None)
      } else {
        val factory = new InstanceFactory(definitionVerifier.get, modelVerifier.get)
        factory.setRegistry(registry)
        factory.newInstance(name, Fragment.composeSingletonIdentity(name)) map (instance => Some(instance))
      }
    }))
  }

  private def removeSingleton(): Future[Any] = {
    registry.autoRemove(name, Fragment.composeSingletonIdentity(name))
  }

  /**
   * <p> Checks if this Fragment is valid by the means of the provided [[codi.verification.ModelVerifier ModelVerifier]]
   * and [[codi.verification.DefinitionVerifier DefinitionVerifier]].
   * <p> TODO as of right now, error logs cannot be propagated apart from throwing exceptions.
   * <p> <strong>The Fragment needs to be unfolded to perform this operation.</strong>
   *
   * @return Boolean - if this Fragment and its [[codi.core.Definition Definition]] is valid
   */
  def isValid: Boolean = {
    if (definitionVerifier.isEmpty || modelVerifier.isEmpty) throw new Exception("No concrete verifiers defined!")
    var res: Boolean = definitionVerifier.get.verify(definition.getRules) && modelVerifier.get.verify(this.createHandle)
    getParents.foreach(fragment => res = res && fragment.isValid)
    res
  }

  /**
   * <p> Package-private [[codi.core.Registry Registry]] getter.
   *
   * @return Registry if available or Exception
   */
  private[core] final def registry: Registry = registryOption.getOrElse(throw new Exception("undefined registry"))

  /**
   * <p> Package-private setter for [[codi.core.Definition Definition]].
   * <p> This operation is by default used by the [[codi.core.TypeFactory TypeFactory]] during Fragment construction only.
   *
   * @param definition the Definition of this Fragment
   */
  private[core] final def setDefinition(definition: Definition): Unit = {
    definitionOption = Some(definition)
    definition.registerObserver(_definitionObserver)
  }

  /**
   * <p> Package-private [[codi.core.Definition Definition]] getter.
   *
   * @return Definition if available or Exception
   */
  private[core] def definition: Definition = definitionOption.getOrElse(throw new Exception("undefined definition"))

  /**
   * <p> Protected [[codi.core.Definition Definition]] getter.
   * <p> This operation is unsafe and expects the Definition to be available!
   *
   * @return Definition if available or Exception
   */
  protected def getDefinition: Definition = definition

  /**
   * <p> Package-private setter for [[codi.core.Registry Registry]].
   * <p> This operation is by default used by the [[codi.core.TypeFactory TypeFactory]] during Fragment construction only.
   *
   * @param registry the Registry used for this Fragment
   */
  private[core] final def setRegistry(registry: Registry): Unit = {
    registryOption = Some(registry)
  }

  /**
   * <p> Package-private setter for [[codi.verification.ModelVerifier ModelVerifier]] and [[codi.verification.DefinitionVerifier DefinitionVerifier]].
   * <p> This operation is by default used by the [[codi.core.TypeFactory TypeFactory]] during Fragment construction only.
   *
   * @param definitionVerifier the DefinitionVerifier this Fragment should use to check its Definition
   * @param modelVerifier      the ModelVerifier this Fragment should use to check its structure
   */
  private[core] final def setVerifiers(definitionVerifier: DefinitionVerifier, modelVerifier: ModelVerifier): Unit = {
    this.modelVerifier = Some(modelVerifier)
    this.definitionVerifier = Some(definitionVerifier)
  }

  /**
   * <p> Create and return a [[codi.core.TypeHandle TypeHandle]].
   * <p> See [[codi.core.TypeHandle TypeHandle]] implementation for more information!
   *
   * @return a [[codi.core.TypeHandle TypeHandle]] created from this Fragment.
   *         Each time, an independent TypeHandle is created and returned.
   */
  final def createHandle: TypeHandle = {
    val isStatic = identity != Fragment.REFERENCE_IDENTITY
    new TypeHandle(fragment = this, isStatic)
  }

  /**
   * <p> Predefined query-method. Gets the set of all type-names the Fragment has in its extension hierarchy.
   * <p> <strong>The Fragment needs to be unfolded to perform this operation.</strong>
   *
   * @return Set[String] - of polymorph type-names
   */
  def getTypeClosure: Set[String] = {
    val results = mutable.Set[String]()
    results.add(name)
    getParents.foreach(parentFragment => results.addAll(parentFragment.getTypeClosure))
    results.toSet
  }

  private[codi] def deepAttributeRuleSet: Set[AttributeRule] = {
    val result = mutable.Set[AttributeRule]()
    result.addAll(definition.getAttributeRules)
    applyDeepResult[AttributeRule](result, getParents.map(parent => parent.deepAttributeRuleSet))
    result.toSet
  }

  private[codi] def deepAssociationRuleSet: Set[AssociationRule] = {
    val result = mutable.Set[AssociationRule]()
    result.addAll(definition.getAssociationRules)
    applyDeepResult[AssociationRule](result, getParents.map(parent => parent.deepAssociationRuleSet))
    result.toSet
  }

  private[codi] def deepValueSet: Set[ConcreteValue] = {
    val result = mutable.Set[ConcreteValue]()
    result.addAll(definition.getConcreteValues)
    applyDeepResult[ConcreteValue](result, getParents.map(parent => parent.deepValueSet))
    result.toSet
  }

  private def applyDeepResult[T <: Rule](result: mutable.Set[T], values: Set[Set[T]]): Unit = {
    values.foreach(ruleSet => {
      ruleSet.foreach(associationRule => {
        val overrideOption = result.find(_.isPolymorphEqual(associationRule))
        if (overrideOption.isEmpty) {
          result.add(associationRule)
        }
      })
    })
  }

  /**
   * <p> Predefined query-method. Gets the set of [[codi.core.rules.AssociationRule AssociationRules]] part of the
   * [[codi.core.Definition Definition]] which target a certain other Fragment i.e. this method returns
   * the different possibility by which a certain other Fragment can be related to this Fragment
   *
   * @param typeName of the related Fragment
   * @return Set[AssociationRule] - between this and the related Fragment
   */
  def getAssociationRulesOfType(typeName: String): Set[AssociationRule] = {
    definition.getAssociationRules.filter(_.targetName == typeName)
  }

  /**
   * <p> The same as [[codi.core.Fragment#getAssociationRulesOfType Fragment.getAssociationRulesOfType()]] but considers
   * the complete extension hierarchy.
   * <p> <strong>The Fragment needs to be unfolded to perform this operation.</strong>
   *
   * @param typeName of the related Fragment
   * @return Set[AssociationRule] - between this (and extensions) and the related Fragment
   */
  def getDeepAssociationRulesOfType(typeName: String): Set[AssociationRule] = {
    val results = mutable.Set[AssociationRule]()
    results.addAll(getAssociationRulesOfType(typeName))
    getParents.foreach(parent => results.addAll(parent.getDeepAssociationRulesOfType(typeName)))
    results.toSet
  }

  /**
   * <p> Predefined query-method. Gets the set of [[codi.core.rules.AssociationRule AssociationRules]] part of the
   * [[codi.core.Definition Definition]] which are variants of the same relation.
   *
   * @param relationName of the association relation to query
   * @return Set[AssociationRule] - regarding a given relation
   */
  def getAssociationRulesOfRelation(relationName: String): Set[AssociationRule] = {
    definition.getAssociationRules.filter(_.associationName == relationName)
  }

  /**
   * <p> The same as [[codi.core.Fragment#getAssociationRulesOfRelation Fragment.getAssociationRulesOfRelation()]] but considers
   * the complete extension hierarchy.
   * <p> <strong>The Fragment needs to be unfolded to perform this operation.</strong>
   *
   * @param relationName of the association relation to query
   * @return Set[AssociationRule] - regarding the given relation
   */
  def getDeepAssociationRulesOfRelation(relationName: String): Set[AssociationRule] = {
    val results = mutable.Set[AssociationRule]()
    results.addAll(getAssociationRulesOfRelation(relationName))
    getParents.foreach(parent => results.addAll(parent.getDeepAssociationRulesOfRelation(relationName)))
    results.toSet
  }

}

/**
 * <p> Contains static values of the Fragment class
 */
object Fragment {

  /**
   * <p> The built-in constant string value used to represent reference identity Fragments.
   */
  val REFERENCE_IDENTITY = "#"

  /**
   * <p> The built-in constant string value used to represent singleton identities. I.e. objects/instances
   * that can and must exist only once in a registry-context.
   */
  val SINGLETON_IDENTITY = "$"

  def composeSingletonIdentity(typeName: String): String = SINGLETON_IDENTITY + "_" + typeName

  def decomposeSingletonIdentity(identity: String): String = {
    identity.split("_")(1)
  }

  def isSingletonIdentity(identity: String): Boolean = identity.startsWith(SINGLETON_IDENTITY)

}
