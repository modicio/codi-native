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
import codi.core.rules.{AssociationRule, AttributeRule, ExtensionRule, RuleDataType}
import codi.core.values.ConcreteValue
import codi.verification.{DefinitionVerifier, ModelVerifier}

/**
 * @param definitionVerifier
 * @param modelVerifier
 */
class TypeFactory
(
  definitionVerifier: DefinitionVerifier,
  modelVerifier: ModelVerifier
) {

  private var registry: Registry = _

  def setRegistry(registry: Registry): Unit = this.registry = registry

  def newType(name: String, identity: String, isTemplate: Boolean): TypeHandle = {
    val definition = new Definition(definitionVerifier)
    val fragment = new Node(name, identity, isTemplate/*, modelVerifier*/)

    fragment.setRegistry(registry)
    fragment.setDefinition(definition)
    fragment.setVerifiers(definitionVerifier, modelVerifier)

    fragment.createHandle
  }

  def loadType(fragmentData: FragmentData, ruleData: Set[RuleData]): TypeHandle = {
    val definition = new Definition(definitionVerifier)
    ruleData.foreach(data => definition.applyRule(loadRule(data)))

    val fragment = new Node(fragmentData.name, fragmentData.identity, fragmentData.isTemplate)

    fragment.setRegistry(registry)
    fragment.setDefinition(definition)
    fragment.setVerifiers(definitionVerifier, modelVerifier)

    fragment.createHandle
  }

  def loadRule(ruleData: RuleData): Rule = {
    ruleData.typeOf match {
      case RuleDataType.ATTRIBUTE => new AttributeRule(ruleData.nativeValue)
      case RuleDataType.VALUE => new ConcreteValue(ruleData.nativeValue)
      case RuleDataType.ASSOCIATION => new AssociationRule(ruleData.nativeValue)
      case RuleDataType.EXTENSION => new ExtensionRule(ruleData.nativeValue)
      case _ => throw new IllegalArgumentException("Cannot determine RuleData typeOf")
    }
  }

}
