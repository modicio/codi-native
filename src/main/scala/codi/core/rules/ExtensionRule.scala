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
package codi.core.rules

import codi.core.Rule

/**
 * <p> A concrete [[codi.core.Rule Rule]] implementation to represent extensions in the native unlinked model.
 * This Rule does not logically distinguish between reference extensions and instantiated extensions.
 * <br />
 * <br />
 * <p> <strong>String format: "ID:PARENT_IDENTITY:PARENT_NAME"</strong>
 * <P> where ID is the unique technical identifier of the [[codi.core.Rule Rule]]
 * <p> where PARENT_IDENTITY is the identity value of the parent [[codi.core.Fragment Fragment]]
 * <p> where TARGET_NAME is the name of the parent [[codi.core.Fragment Fragment]]
 *
 * @see [[codi.core.Rule]]<p>[[codi.core.datamappings.RuleData]]
 * @param nativeValue the string representation in the native-language format
 */
class ExtensionRule(nativeValue: String) extends Rule(nativeValue) {

  val parentName: String = parseParentName(nativeValue)
  val parentIdentity: String = parseParentIdentity(nativeValue)

  /**
   * <p>Helper to retrieve the parent name from the serialised value
   *
   * @param nativeValue serialised rule representation
   * @return String of parent name
   */
  private def parseParentName(nativeValue: String): String = nativeValue.split(":")(2)

  /**
   * <p>Helper to retrieve the parent identity from the serialised value
   *
   * @param nativeValue serialised rule representation
   * @return String of parent identity
   */
  private def parseParentIdentity(nativeValue: String): String = nativeValue.split(":")(1)

  /**
   * <p>Implementation of [[codi.core.Rule#serialise Rule.serialise()]]
   *
   * @return String of serialised rule
   */
  override def serialise(): String = {
    id + ":" + parentIdentity + ":" + parentName
  }

  /**
   * <p>Implementation of [[codi.core.Rule#serialiseSimple Rule.serialiseSimple()]].
   * <p>This method must only be used for human-readable logs and outputs and not for technical purposes!
   *
   * @return String of simplified serialisation
   */
  override def serialiseSimple(): String = {
    "..." + id.takeRight(5) + ":" + parentIdentity + ":" + parentName
  }

  /**
   * <p>Implementation of [[codi.core.Rule#verify Rule.verify()]]
   * <p>FIXME not implemented yet, returns always true
   *
   * @return Boolean - if the rule is valid in terms of producing a valid serialisation
   */
  override def verify(): Boolean = {
    true
  }

  /**
   * <p>Implementation of [[codi.core.Rule#fork Rule.fork()]].
   * <p> In this case of an ExtensionRule, especially the parent identity (which is in the reference case set to the reference identity)
   * is replaced by the provided new identity of the instantiation.
   *
   * @param identity the identity of an instantiated [[codi.core.Fragment Fragment]]
   * @return [[codi.core.Rule Rule]] - copy of this Rule with changed identity value and new ID
   */
  override def fork(identity: String): Rule = ExtensionRule.create(parentName, identity, Some(Rule.UNKNOWN_ID))

  override def getDataType: Int = RuleDataType.EXTENSION

}

/**
 * <p> ExtensionRule companion object for the static factory creator.
 *
 * @see [[codi.core.rules.ExtensionRule ExtensionRule]]
 */
object ExtensionRule {

  /**
   * <p> Create a new [[codi.core.rules.ExtensionRule ExtensionRule]] from raw data.
   * <p> This serves as a factory method for the ExtensionRule.
   * <p> If an empty idOption is provided, the id is set to [[codi.core.Rule#UNKNOWN_ID UNKNOWN_ID]] and must be
   * changed manually.
   *
   * @param parentName     name of the parent [[codi.core.Fragment Fragment]]
   * @param parentIdentity identity of the parent [[codi.core.Fragment Fragment]]
   * @param idOption       id value if known, set to default otherwise
   * @return ExtensionRule created from provided values
   */
  def create(parentName: String, parentIdentity: String, idOption: Option[String] = None): ExtensionRule = {
    var id = Rule.UNKNOWN_ID
    if (idOption.isDefined) id = idOption.get
    new ExtensionRule(id + ":" + parentIdentity + ":" + parentName)
  }
}