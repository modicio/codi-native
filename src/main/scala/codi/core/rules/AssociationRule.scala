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
 * <p> A concrete [[codi.core.Rule Rule]] implementation to represent associations in the native unlinked model.
 * <br />
 * <br />
 * <p> <strong>String format: "ID:ASSOCIATION_NAME:TARGET_NAME:MULTIPLICITY"</strong>
 * <P> where ID is the unique technical identifier of the [[codi.core.Rule Rule]]
 * <p> where ASSOCIATION_NAME is the arbitrary name of the relation
 * <p> where TARGET_NAME is the name of the [[codi.core.Fragment Fragment]] type that can be associated
 * <P> where MULTIPLICITY is ["*" | "u_int" | "u_int...u_int" | "u_int...*"]
 *
 * @see [[codi.core.Rule]]<p>[[codi.core.datamappings.RuleData]]
 * @param nativeValue the string representation in the native-language format
 */
class AssociationRule(nativeValue: String) extends Rule(nativeValue) {

  val associationName: String = parseName(nativeValue)
  val targetName: String = parseTarget(nativeValue)
  val multiplicity: String = parseMultiplicity(nativeValue)

  /**
   * <p>Helper to retrieve the association name from the serialised value
   *
   * @param nativeValue serialised rule representation
   * @return String of association name
   */
  private def parseName(nativeValue: String): String = nativeValue.split(":")(1)

  /**
   * <p>Helper to retrieve the target name from the serialised value
   *
   * @param nativeValue serialised rule representation
   * @return String of target name
   */
  private def parseTarget(nativeValue: String): String = nativeValue.split(":")(2)

  /**
   * <p>Helper to retrieve the multiplicity from the serialised value
   * TODO multiplicities should be covered by a separate class and an numeric representation
   *
   * @param nativeValue serialised rule representation
   * @return String of multiplicity
   */
  private def parseMultiplicity(nativeValue: String): String = nativeValue.split(":")(3)

  /**
   * <p>Implementation of [[codi.core.Rule#serialise Rule.serialise()]]
   *
   * @return String of serialised rule
   */
  override def serialise(): String = {
    id + ":" + associationName + ":" + targetName + ":" + multiplicity
  }

  /**
   * <p>Implementation of [[codi.core.Rule#serialiseSimple Rule.serialiseSimple()]].
   * <p>This method must only be used for human-readable logs and outputs and not for technical purposes!
   *
   * @return String of simplified serialisation
   */
  override def serialiseSimple(): String = {
    "..." + id.takeRight(5) + ":" + associationName + ":" + targetName + ":" + multiplicity
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
   *
   * @param identity the identity of an instantiated [[codi.core.Fragment Fragment]]
   * @return [[codi.core.Rule Rule]] - copy of this Rule with changed identity value and new ID
   */
  override def fork(identity: String): Rule = AssociationRule.create(associationName, targetName, multiplicity, Some(Rule.UNKNOWN_ID))

  /**
   *
   * @param rule
   * @return
   */
  override def isPolymorphEqual(rule: Rule): Boolean = {
    rule match {
      case rule: AssociationRule => {
        //TODO some sophisticated reasoning must be made here!
        // it has especially to be checked that the target is a child of the parent target
        rule.associationName == associationName && rule.multiplicity == multiplicity && targetName == targetName
      }
      case _ => false
    }
  }

  override def getDataType: Int = RuleDataType.ASSOCIATION

  /**
   *
   * @return
   */
  def hasIntMultiplicity: Boolean = multiplicity.toIntOption.isDefined

  /**
   *
   * @return
   */
  def getIntMultiplicity: Int = {
    if(!hasIntMultiplicity){
      throw new UnsupportedOperationException("Cannot convert non-int multiplicity to int")
    }else{
      multiplicity.toIntOption.get
    }
  }

}

/**
 * <p> AssociationRule companion object for the static factory creator.
 *
 * @see [[codi.core.rules.AssociationRule AssociationRule]]
 */
object AssociationRule {

  /**
   * <p> Create a new [[codi.core.rules.AssociationRule AssociationRule]] from raw data.
   * <p> This serves as a factory method for the AssociationRule.
   * <p> If an empty idOption is provided, the id is set to [[codi.core.Rule#UNKNOWN_ID UNKNOWN_ID]] and must be
   * changed manually.
   *
   * @param associationName name of the association relation
   * @param target          name of the target [[codi.core.Fragment Fragment]] type to associate
   * @param multiplicity    multiplicity value, see [[codi.core.rules.AssociationRule AssociationRule]]
   * @param idOption        id value if known, set to default otherwise
   * @return AssociationRule created from provided values
   */
  def create(associationName: String, target: String, multiplicity: String, idOption: Option[String] = None): AssociationRule = {
    var id = Rule.UNKNOWN_ID
    if (idOption.isDefined) id = idOption.get
    new AssociationRule(id + ":" + associationName + ":" + target + ":" + multiplicity)
  }
}
