export var NumberStyles;function validResponse(e,t){const[,r,n,i]=e;return{sign:r||"",prefix:n||"",digits:i,radix:t}}function getRange(e,t){switch(t){case 8:return e?[0,255]:[-128,127];case 16:return e?[0,65535]:[-32768,32767];case 32:return e?[0,4294967295]:[-2147483648,2147483647];default:throw new Error("Invalid bit size.")}}function getInvalidDigits(e){switch(e){case 2:return/[^0-1]/;case 8:return/[^0-7]/;case 10:return/[^0-9]/;case 16:return/[^0-9a-fA-F]/;default:throw new Error("Invalid Base.")}}function getRadix(e,t){if(t&NumberStyles.AllowHexSpecifier)return 16;switch(e){case"0b":case"0B":return 2;case"0o":case"0O":return 8;case"0x":case"0X":return 16;default:return 10}}!function(e){e[e.AllowHexSpecifier=512]="AllowHexSpecifier"}(NumberStyles||(NumberStyles={}));export function isValid(e,t,r){const n=/^\s*([\+\-])?(0[xXoObB])?([0-9a-fA-F]+)\s*$/.exec(e.replace(/_/g,""));if(null!=n){const[,,e,i]=n;if(!getInvalidDigits(r=r||getRadix(e,t)).test(i))return validResponse(n,r)}return null}export function parse(e,t,r,n,i){const s=isValid(e,t,i);if(null!=s){let e=Number.parseInt(s.sign+s.digits,s.radix);if(!Number.isNaN(e)){const[t,i]=getRange(!0,n);!r&&10!==s.radix&&e>=t&&e<=i&&(e=e<<32-n>>32-n);const[a,o]=getRange(r,n);if(e>=a&&e<=o)return e}}throw new Error("Input string was not in a correct format.")}export function tryParse(e,t,r,n,i){try{return i.contents=parse(e,t,r,n),!0}catch(e){return!1}}export function op_UnaryNegation_Int8(e){return-128===e?e:-e}export function op_UnaryNegation_Int16(e){return-32768===e?e:-e}export function op_UnaryNegation_Int32(e){return-2147483648===e?e:-e}export function divRem(e,t,r){const n=~~(e/t),i=e%t;return void 0===r?[n,i]:(r.contents=i,n)}