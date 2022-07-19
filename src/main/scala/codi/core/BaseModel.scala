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

import codi.core.datamappings.{FragmentData, RuleData}
import codi.core.rules.ExtensionRule
import codi.verification.DefinitionVerifier

import scala.concurrent.Future

/**
 * <p> The BaseModel is a semi-concrete implementation of a [[codi.core.Fragment Fragment]]. A BaseModel serves as a
 * design-time implementation of a invariant type-fragment which is represented by a concrete implementation of the
 * BaseModel.
 * <p> The BaseModel implements the [[codi.core.Base Base]] trait but does not override its methods. Those must be implemented
 * by concrete hook classes representing concrete types. <strong>Not the the methods enforced by the Base tait must provide
 * constant returns. Those must be available at instantiation time.</strong>
 * <p> A concrete implementation must call the setup method at construction time.
 * <br />
 * <br />
 * <p> Additionally, a BaseModel enforces certain behaviour by final overrides: isNode is set to false and the parent set is
 * set to empty, consequently a BaseModel can have no extensions; applyRule and removeRule throw an exception in any case,
 * consequently a BaseModel is invariant.
 *
 * @see [[codi.core.Fragment]]<p>[[codi.core.Base]]
 * @param name       constant name of the [[codi.core.Fragment Fragment]] type.
 * @param identity   constant identity of the [[codi.core.Fragment Fragment]] type. Following intended usage, the identity must
 *                   always be set to the [[codi.core.Fragment#REFERENCE_IDENTITY REFERENCE_IDENTITY]] value of the Fragment
 *                   object.
 * @param isTemplate flag if this type is instantiable or abstract, see [[codi.core.Fragment Fragment]]
 */
abstract class BaseModel(name: String, identity: String, isTemplate: Boolean) extends Fragment(name, identity, isTemplate) with Base {

  /**
   * <p>Applies a [[codi.core.Registry Registry]] and [[codi.verification.DefinitionVerifier DefinitionVerifier]] to the BaseModel.
   * <p>This method must be called before first use / directly after construction.
   * <p>This method also adds the concrete BaseModel to the provided registry to make it available for all other members
   * of the runtime-environment.
   *
   * @param registry           the initialised [[codi.core.Registry Registry]] used by the runtime-environment
   * @param definitionVerifier the selected [[codi.verification.DefinitionVerifier DefinitionVerifier]] used by the runtime environment
   * @return Future[Unit]
   */
  def setup(registry: Registry, definitionVerifier: DefinitionVerifier): Future[Unit] = {
    super.setRegistry(registry)
    super.setDefinition({
      val definition = new Definition(definitionVerifier)
      (getAssociationRules ++ getAttributeRules).foreach(definition.applyRule)
      definition
    })
    registry.setType(createHandle)
  }

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#isNode Fragment#isNode()]].
   *
   * @return Boolean - always false
   */
  override final def isNode: Boolean = false

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#isValid Fragment#isValid()]].
   * <p>If no [[codi.verification.ModelVerifier ModelVerifier]] is specified, an exception is thrown.
   *
   * @return Boolean - if model is verified according to the [[codi.verification.ModelVerifier ModelVerifier]]
   */
  override final def isValid: Boolean = {
    if (modelVerifier.isEmpty) throw new Exception("No verifier specified!")
    modelVerifier.get.verify(createHandle)
  }

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#getParents Fragment#getParents()]].
   *
   * @return Set[Fragment] - always an empty set
   */
  override final def getParents: Set[Fragment] = Set()

  override final def getExtensionRules: Set[ExtensionRule] = Set()

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#applyRule Fragment#applyRule()]].
   * <p>This operation is forbidden in BaseModel an throws an according Exception.
   *
   * @param rule the [[codi.core.Rule Rule]] to apply
   */
  override final def applyRule(rule: Rule): Unit = {
    throw new Exception("forbidden: unable to apply rule to base model")
  }

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#removeRule Fragment#removeRule()]].
   * <p>This operation is forbidden in BaseModel an throws an according Exception.
   *
   * @param rule the [[codi.core.Rule Rule]] to remove
   */
  override final def removeRule(rule: Rule): Unit = {
    throw new Exception("forbidden: unable to remove rule from base model")
  }

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#unfold Fragment#unfold()]].
   * <p>This method calls super.unfold() and triggers the onModelChange callback / template method.
   *
   * @return Future[Unit] or Exception
   */
  override final def unfold(): Future[Unit] = {
    super.unfold()
    onModelChange()
  }

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#unfold Fragment#unfold()]].
   * <p>Only delegates to super call.
   */
  override final def fold(): Unit = {
    super.fold()
  }

  /**
   * <p>Abstract callback invoked if the model changes or is unfolded.
   * <p>The method is invoked asynchronously and can invoke any (long-taking) operation. However, the control-flow may
   * be interrupted if the method does not return.
   *
   * @return Future[Unit]
   */
  def onModelChange(): Future[Unit]

  /**
   * <p>Concrete final implementation of [[codi.core.Fragment#toData Fragment#toData()]].
   * <p>FIXME not implement yet
   *
   * @return Tuple of [[codi.core.Fragment Fragment]] data representation.
   */
  override final private[codi] def toData: (FragmentData, Set[RuleData]) = {
    (null, Set())
  }

}
