import{Record,Union}from"./Types.js";import{combineHashCodes,equalArraysWith,stringHash}from"./Util.js";import Decimal from"./Decimal.js";import{fromInt as int64FromInt}from"./Long.js";export class CaseInfo{constructor(e,n,t,r){this.declaringType=e,this.tag=n,this.name=t,this.fields=r}}export class MethodInfo{constructor(e,n,t){this.name=e,this.parameters=n,this.returnType=t}}export class TypeInfo{constructor(e,n,t,r,o,u,s){this.fullname=e,this.generics=n,this.construct=t,this.parent=r,this.fields=o,this.cases=u,this.enumCases=s}toString(){return fullName(this)}GetHashCode(){return getHashCode(this)}Equals(e){return equals(this,e)}}export class GenericParameter extends TypeInfo{constructor(e){super(e)}}export function getGenerics(e){return null!=e.generics?e.generics:[]}export function getHashCode(e){const n=stringHash(e.fullname),t=getGenerics(e).map(getHashCode);return combineHashCodes([n,...t])}export function equals(e,n){return""===e.fullname?""===n.fullname&&equalArraysWith(getRecordElements(e),getRecordElements(n),(([e,n],[t,r])=>e===t&&equals(n,r))):e.fullname===n.fullname&&equalArraysWith(getGenerics(e),getGenerics(n),equals)}export function class_type(e,n,t,r){return new TypeInfo(e,n,t,r)}export function record_type(e,n,t,r){return new TypeInfo(e,n,t,void 0,r)}export function anonRecord_type(...e){return new TypeInfo("",void 0,void 0,void 0,(()=>e))}export function union_type(e,n,t,r){const o=new TypeInfo(e,n,t,void 0,void 0,(()=>{const e=t.prototype.cases();return r().map(((n,t)=>new CaseInfo(o,t,e[t],n)))}));return o}export function tuple_type(...e){return new TypeInfo("System.Tuple`"+e.length,e)}export function delegate_type(...e){return new TypeInfo("System.Func`"+e.length,e)}export function lambda_type(e,n){return new TypeInfo("Microsoft.FSharp.Core.FSharpFunc`2",[e,n])}export function option_type(e){return new TypeInfo("Microsoft.FSharp.Core.FSharpOption`1",[e])}export function list_type(e){return new TypeInfo("Microsoft.FSharp.Collections.FSharpList`1",[e])}export function array_type(e){return new TypeInfo("[]",[e])}export function enum_type(e,n,t){return new TypeInfo(e,[n],void 0,void 0,void 0,void 0,t)}export function measure_type(e){return new TypeInfo(e)}export function generic_type(e){return new GenericParameter(e)}export const obj_type=new TypeInfo("System.Object");export const unit_type=new TypeInfo("Microsoft.FSharp.Core.Unit");export const char_type=new TypeInfo("System.Char");export const string_type=new TypeInfo("System.String");export const bool_type=new TypeInfo("System.Boolean");export const int8_type=new TypeInfo("System.SByte");export const uint8_type=new TypeInfo("System.Byte");export const int16_type=new TypeInfo("System.Int16");export const uint16_type=new TypeInfo("System.UInt16");export const int32_type=new TypeInfo("System.Int32");export const uint32_type=new TypeInfo("System.UInt32");export const int64_type=new TypeInfo("System.Int64");export const uint64_type=new TypeInfo("System.UInt64");export const int128_type=new TypeInfo("System.Int128");export const uint128_type=new TypeInfo("System.UInt128");export const nativeint_type=new TypeInfo("System.IntPtr");export const unativeint_type=new TypeInfo("System.UIntPtr");export const float16_type=new TypeInfo("System.Half");export const float32_type=new TypeInfo("System.Single");export const float64_type=new TypeInfo("System.Double");export const decimal_type=new TypeInfo("System.Decimal");export const bigint_type=new TypeInfo("System.Numerics.BigInteger");export function name(e){if(Array.isArray(e))return e[0];if(e instanceof TypeInfo){const n=getElementType(e);if(null!=n)return name(n)+"[]";{const n=e.fullname.lastIndexOf(".");return-1===n?e.fullname:e.fullname.substr(n+1)}}return e.name}export function fullName(e){const n=getElementType(e);return null!=n?fullName(n)+"[]":null==e.generics||0===e.generics.length?e.fullname:e.fullname+"["+e.generics.map((e=>fullName(e))).join(",")+"]"}export function namespace(e){const n=getElementType(e);if(null!=n)return namespace(n);{const n=e.fullname.lastIndexOf(".");return-1===n?"":e.fullname.substr(0,n)}}export function isArray(e){return null!=getElementType(e)}export function getElementType(e){var n;return"[]"===e.fullname&&1===(null===(n=e.generics)||void 0===n?void 0:n.length)?e.generics[0]:void 0}export function isGenericType(e){return null!=e.generics&&e.generics.length>0}export function isGenericParameter(e){return e instanceof GenericParameter}export function isEnum(e){return null!=e.enumCases&&e.enumCases.length>0}export function isSubclassOf(e,n){return n.fullname===obj_type.fullname||null!=e.parent&&(e.parent.Equals(n)||isSubclassOf(e.parent,n))}function isErasedToNumber(e){return isEnum(e)||[int8_type.fullname,uint8_type.fullname,int16_type.fullname,uint16_type.fullname,int32_type.fullname,uint32_type.fullname,float32_type.fullname,float64_type.fullname].includes(e.fullname)}export function isInstanceOfType(e,n){if(e.fullname===obj_type.fullname)return!0;switch(typeof n){case"boolean":return e.fullname===bool_type.fullname;case"string":return e.fullname===string_type.fullname;case"function":return isFunction(e);case"number":return isErasedToNumber(e);default:return null!=e.construct&&n instanceof e.construct}}export function getGenericTypeDefinition(e){return null==e.generics?e:new TypeInfo(e.fullname,e.generics.map((()=>obj_type)))}export function getEnumUnderlyingType(e){var n;return null===(n=e.generics)||void 0===n?void 0:n[0]}export function getEnumValues(e){if(isEnum(e)&&null!=e.enumCases)return e.enumCases.map((e=>e[1]));throw new Error(`${e.fullname} is not an enum type`)}export function getEnumNames(e){if(isEnum(e)&&null!=e.enumCases)return e.enumCases.map((e=>e[0]));throw new Error(`${e.fullname} is not an enum type`)}function getEnumCase(e,n){if(null!=e.enumCases){if("string"==typeof n){for(const t of e.enumCases)if(t[0]===n)return t;throw new Error(`'${n}' was not found in ${e.fullname}`)}for(const t of e.enumCases)if(t[1]===n)return t;return["",n]}throw new Error(`${e.fullname} is not an enum type`)}export function parseEnum(e,n){const t=parseInt(n,10);return getEnumCase(e,isNaN(t)?n:t)[1]}export function tryParseEnum(e,n,t){try{return t.contents=parseEnum(e,n),!0}catch(e){return!1}}export function getEnumName(e,n){return getEnumCase(e,n)[0]}export function isEnumDefined(e,n){try{const t=getEnumCase(e,n);return null!=t[0]&&""!==t[0]}catch(e){}return!1}export function getUnionCases(e){if(null!=e.cases)return e.cases();throw new Error(`${e.fullname} is not an F# union type`)}export function getRecordElements(e){if(null!=e.fields)return e.fields();throw new Error(`${e.fullname} is not an F# record type`)}export function getTupleElements(e){if(isTuple(e)&&null!=e.generics)return e.generics;throw new Error(`${e.fullname} is not a tuple type`)}export function getFunctionElements(e){if(isFunction(e)&&null!=e.generics){const n=e.generics;return[n[0],n[1]]}throw new Error(`${e.fullname} is not an F# function type`)}export function isUnion(e){return e instanceof TypeInfo?null!=e.cases:e instanceof Union}export function isRecord(e){return e instanceof TypeInfo?null!=e.fields:e instanceof Record}export function isTuple(e){return e.fullname.startsWith("System.Tuple")}export function isFunction(e){return"Microsoft.FSharp.Core.FSharpFunc`2"===e.fullname}export function getUnionFields(e,n){const t=getUnionCases(n)[e.tag];if(null==t)throw new Error(`Cannot find case ${e.name} in union type`);return[t,e.fields]}export function getUnionCaseFields(e){return null==e.fields?[]:e.fields}export function getRecordFields(e){return Object.keys(e).map((n=>e[n]))}export function getRecordField(e,n){return e[n[0]]}export function getTupleFields(e){return e}export function getTupleField(e,n){return e[n]}export function makeUnion(e,n){const t=(e.fields||[]).length;if(n.length!==t)throw new Error(`Expected an array of length ${t} but got ${n.length}`);return null!=e.declaringType.construct?new e.declaringType.construct(e.tag,n):{}}export function makeRecord(e,n){const t=getRecordElements(e);if(t.length!==n.length)throw new Error(`Expected an array of length ${t.length} but got ${n.length}`);return null!=e.construct?new e.construct(...n):t.reduce(((e,[t,r],o)=>(e[t]=n[o],e)),{})}export function makeTuple(e,n){return e}export function makeGenericType(e,n){return new TypeInfo(e.fullname,n,e.construct,e.parent,e.fields,e.cases)}export function createInstance(e,n){if("function"==typeof e.construct)return new e.construct(...null!=n?n:[]);if(isErasedToNumber(e))return 0;switch(e.fullname){case obj_type.fullname:return{};case bool_type.fullname:return!1;case"System.Int64":case"System.UInt64":return int64FromInt(0);case decimal_type.fullname:return new Decimal(0);case char_type.fullname:return null;default:throw new Error(`Cannot access constructor of ${e.fullname}`)}}export function getValue(e,n){return n[e[0]]}function assertUnion(e){if(!(e instanceof Union))throw new Error("Value is not an F# union type")}export function getCaseTag(e){return assertUnion(e),e.tag}export function getCaseName(e){return assertUnion(e),e.cases()[e.tag]}export function getCaseFields(e){return assertUnion(e),e.fields}