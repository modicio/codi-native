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

import codi.core.rules._

/**
 * <p> The Base trait defines any class producing or forwarding a [[codi.core.Definition Definition]] compatible specification
 * of [[codi.core.Rule Rules]].
 *
 * <p> As of right now, [[codi.core.rules.ConstraintRule]] and [[codi.core.rules.BehaviourRule]] are not supported, i.e.
 * throw an [[UnsupportedOperationException]].
 */
trait Base {

  /**
   * <p>Get all [[codi.core.rules.AttributeRule AttributeRules]] of the target.
   *
   * @return Set[AttributeRule]
   */
  def getAttributeRules: Set[AttributeRule]

  /**
   * <p>Get all [[codi.core.rules.AssociationRule AssociationRules]] of the target.
   *
   * @return Set[AssociationRule]
   */
  def getAssociationRules: Set[AssociationRule]

  /**
   * <p>Get all [[codi.core.rules.ExtensionRule ExtensionRules]] of the target.
   *
   * @return Set[ExtensionRule]
   */
  def getExtensionRules: Set[ExtensionRule]

  /**
   * <p>Get all [[codi.core.rules.ConstraintRule ConstraintRules]] of the target.
   * <p>Throws always an [[UnsupportedOperationException]]!
   *
   * @return Set[ConstraintRule]
   */
  def getConstraintRules: Set[ConstraintRule] = ???

  /**
   * <p>Get all [[codi.core.rules.BehaviourRule BehaviourRules]] of the target.
   * <p>Throws always an [[UnsupportedOperationException]]!
   *
   * @return Set[BehaviourRule]
   */
  def getBehaviourRules: Set[BehaviourRule] = ???
}
