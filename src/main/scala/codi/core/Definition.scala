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

import codi.core.datamappings.RuleData
import codi.core.rules._
import codi.util.Observable
import codi.verification.DefinitionVerifier

import scala.collection.mutable

/**
 * <p> The Definition class is a final concrete [[codi.core.Base Base]] implementation representing the set of
 * [[codi.core.Rule Rules]] a [[codi.core.Fragment Fragment]] possesses.
 * <p> The Definition encapsulates the Rules in individual sets and provides an api to edit the rule-set. Note that the
 * Definition class itself does not ensure validity of the resulting model, especially in an extension-hierarchy. If a new Rule is
 * added or an old Rule is removed, the provided [[codi.verification.DefinitionVerifier DefinitionVerifier]] is called with
 * the temporary altered rule-set. If the DefinitionVerifier accepts accepts the rule-set as valid, the changes are applied.
 *
 * <p> The [[codi.util.Observable Observable]] trait is implemented in this class to enable clients to observe rule-set changes
 * if Rules are added and removed.
 *
 * @param definitionVerifier the [[codi.verification.DefinitionVerifier DefinitionVerifier]] to allow or reject modifications of the
 *                           rule-set.
 */
final class Definition
(
  val definitionVerifier: DefinitionVerifier
) extends Observable with Base {

  private val attributes: mutable.Set[AttributeRule] = mutable.Set()
  private val extensions: mutable.Set[ExtensionRule] = mutable.Set()
  private val associations: mutable.Set[AssociationRule] = mutable.Set()

  /**
   * <p> Get all [[codi.core.Rule Rules]] part of this Definition.
   *
   * @return Set[codi.core.Rule] - all Rules part of the Definition
   */
  def getRules: Set[Rule] = Set.from(attributes ++ extensions ++ associations)

  /**
   * <p> Generates the set of [[codi.core.datamappings.RuleData RuleData]] which contains all [[codi.core.Rule Rules]] of
   * this Definition in their serialised form.
   * <p> The name and identity parameters are required because the Definition does not know by which [[codi.core.Fragment Fragment]]
   * it is used. However the RuleData serialisation requires this information.
   *
   * @param name     name of the [[codi.core.Fragment Fragment]] possessing this Definition
   * @param identity identity of the [[codi.core.Fragment Fragment]] possessing this Definition
   * @return Set[RuleData] - all Rules part of the Definition in their serialised form
   */
  def toData(name: String, identity: String): Set[RuleData] = {
    getRules.map(rule => RuleData(rule.id, name, identity, rule.serialise()))
  }

  /**
   * <p> Fork this Definition according to the overall fork specification. This operation produces a deep copy
   * of the Definition and all its [[codi.core.Rule Rules]].
   * <p> This operation takes the identity parameter because [[codi.core.rules.ExtensionRule ExtensionRules]] target a
   * specified identity which must be exchanged during the fork.
   * <p> This operation calls [[codi.core.Rule#fork Rule.fork(identity)]] on each rule.
   *
   * @param identity the identifier of the forked [[codi.core.Fragment Fragment]] following the default usage.
   * @return Definition - the forked deep-copy of this Definition
   */
  def fork(identity: String): Definition = {
    val newDefinition = new Definition(definitionVerifier)
    getRules.map(_.fork(identity)).foreach(newDefinition.applyRule)
    newDefinition
  }

  /**
   * <p>Add a [[codi.core.Rule Rule]] to the rule-set represented by this Definition.
   * <p> This operation creates a temporary copy of the rule-set and the provided [[codi.verification.DefinitionVerifier DefinitionVerifier]]
   * decides if the new rule is allowed. If true, thr rule is added to the rule-set and all observers are notified.
   * <p> <strong>Note as of right now, only [[codi.core.rules.AttributeRule AttributeRules]], [[codi.core.rules.AssociationRule AssociationRules]]
   * and [[codi.core.rules.ExtensionRule ExtensionRules]] are supported.</strong>
   *
   * TODO provide feedback by returning a success criteria
   *
   * @param rule the new [[codi.core.Rule Rule]] to add
   */
  private[core] def applyRule(rule: Rule): Unit = {
    val isValid = definitionVerifier.verify(getRules + rule)
    if (isValid) {
      rule match {
        case rule: AttributeRule => attributes.add(rule)
        case rule: ExtensionRule => extensions.add(rule)
        case rule: AssociationRule => associations.add(rule)
      }
      notifyObservers()
    }
  }

  /**
   * <p> Remove a [[codi.core.Rule Rule]] from the rule-set represented by this Definition.
   * <p> This operation creates a temporary copy of the rule-set and the provided [[codi.verification.DefinitionVerifier DefinitionVerifier]]
   * decides if the deletion is allowed. If true, thr rule is removed from the rule-set and all observers are notified.
   * <p> <strong>Note as of right now, only [[codi.core.rules.AttributeRule AttributeRules]], [[codi.core.rules.AssociationRule AssociationRules]]
   * and [[codi.core.rules.ExtensionRule ExtensionRules]] are supported.</strong>
   *
   * TODO provide feedback by returning a success criteria
   *
   * @param rule the [[codi.core.Rule Rule]] to remove
   */
  private[core] def removeRule(rule: Rule): Unit = {
    val rules = getRules
    val newRuleset = rules.filter(_.id != rule.id)
    if (definitionVerifier.verify(newRuleset)) {
      rule match {
        case rule: AttributeRule => attributes.remove(rule)
        case rule: ExtensionRule => extensions.remove(rule)
        case rule: AssociationRule => associations.remove(rule)
      }
      notifyObservers()
    }
  }

  /**
   * <p> Remove a [[codi.core.Rule Rule]] from the rule-set represented by this Definition.
   * <p> <strong>See [[codi.core.Definition#removeRule Definition.removeRule()]] for more information!</strong>
   * <p> Note that in certain usecases, unique ids are not yet provided by the individual Rules and this operation
   * may lead to unexpected results. See [[codi.core.Rule Rule (autoId)]] for more information.
   *
   * @param ruleID id of the [[codi.core.Rule Rule]] to remove
   */
  private[core] def removeRuleByID(ruleID: String): Unit = {
    val rule = getRules.find(_.id == ruleID)
    if (rule.isDefined) removeRule(rule.get)
  }

  /**
   * <p> Get all [[codi.core.rules.AttributeRule AttributeRules]] part of this Definition.
   * <p> The provided result is deep-immutable and safe.
   *
   * @return Set[AttributeRule] - immutable set of all [[codi.core.rules.AttributeRule AttributeRules]]
   */
  override def getAttributeRules: Set[AttributeRule] = Set.from(attributes)

  /**
   * <p> Get all [[codi.core.rules.AssociationRule AssociationRules]] part of this Definition.
   * <p> The provided result is deep-immutable and safe.
   *
   * @return Set[AssociationRule] - immutable set of all [[codi.core.rules.AssociationRule AssociationRules]]
   */
  override def getAssociationRules: Set[AssociationRule] = Set.from(associations)

  /**
   * <p> Get all [[codi.core.rules.ExtensionRule ExtensionRules]] part of this Definition.
   * <p> The provided result is deep-immutable and safe.
   *
   * @return Set[ExtensionRule] - immutable set of all [[codi.core.rules.ExtensionRule ExtensionRules]]
   */
  override def getExtensionRules: Set[ExtensionRule] = Set.from(extensions)

  /**
   * <p> Not implemented yet. Returns an empty set always
   *
   * TODO
   *
   * @return Set[ConstraintRule] - empty set
   */
  override def getConstraintRules: Set[ConstraintRule] = Set()

  /**
   * <p> Not implemented yet. Returns an empty set always
   *
   * TODO
   *
   * @return Set[BehaviourRule] - empty set
   */
  override def getBehaviourRules: Set[BehaviourRule] = Set()
}