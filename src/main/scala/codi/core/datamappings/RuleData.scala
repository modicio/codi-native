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
package codi.core.datamappings

import codi.core.Rule

/**
 * <p> Tuple to represent a [[codi.core.Rule Rule]] for serialisation.
 * This class can contain any concrete Rule.
 * This class does not differentiate between instantiated and reference Rules.
 *
 * @param id           technical identifier of the rule. The [[codi.core.Rule Rule]] may assign automatically generated values.
 *                     if no such value exist, the id has the value of [[codi.core.Rule#UNKNOWN_ID UNKNOWN_ID]] and must be
 *                     assigned manually.
 * @param fragmentName name of the [[codi.core.Fragment Fragment]] this rule is part of
 * @param identity     identity of the [[codi.core.Fragment Fragment]] this rule is part of
 * @param nativeValue  serialised value of the [[codi.core.Rule Rule]]. Depending on the serialised format, all
 *                     different concrete rules must be distinguishable
 */
case class RuleData(id: String, fragmentName: String, identity: String, nativeValue: String, typeOf: Int)