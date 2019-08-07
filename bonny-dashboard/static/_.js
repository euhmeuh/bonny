"use strict";
const _ = (function(){
  function get(element, key, defaultValue) {
    return (element && element[key]) ?
      element[key] :
      defaultValue;
  }

  function forLoop(collection, func) {
    (collection || []).forEach(func);
  }

  function pairs(dict) {
    return dict ?
      Object.entries(dict).map((kv) => ({ "key": kv[0], "value": kv[1] })) :
      [];
  }

  function templateString(string, context) {
    return _.pairs(context)
      .reduce((str, val) => str.replace("{" + val.key + "}", val.value), string);
  }

  function capitalize(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
  }

  function toggle(currentValue, possibleValues) {
    const index = possibleValues.indexOf(currentValue);
    const nextValue = possibleValues[index+1];
    return nextValue || possibleValues[0];
  }

  function parseArray(string) {
    const value = eval(string);
    return Array.isArray(value) ? value : [];
  }

  return { get, "for": forLoop, pairs, templateString, capitalize, toggle, parseArray };
}());
