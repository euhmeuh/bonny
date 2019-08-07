"use strict";
const Page = (function(){
  function onload(func) {
    document.readyState === "loading" ?
      document.addEventListener("DOMContentLoaded", func) :
      func();
  }

  function get(query, parent) {
    return (parent || document).querySelector(query);
  }

  function getAll(query, parent) {
    return Array.from((parent || document).querySelectorAll(query));
  }

  function evalTemplateText(value, context) {
    return _.templateString(value, context);
  }

  function evalTemplate(element, context) {
    if (element.nodeType === Node.TEXT_NODE) {
      element.nodeValue = evalTemplateText(element.nodeValue, context);
    } else {
      _.for(Array.from(element.attributes || []), attr => {
        element.setAttribute(attr.name, evalTemplateText(attr.value, context));
      });
      _.for(Array.from(element.childNodes || []), child => {
        evalTemplate(child, context);
      });
    }
  }

  function appendChildrenFromTemplate(parent, template, context) {
    const element = template.cloneNode(true);
    _.for(Array.from(element.childNodes || []), child => {
      evalTemplate(child, context, (x) => x);
      parent.appendChild(child);
    });
  }

  function createElementFromJson(jsonElement) {
    const element = document.createElement(jsonElement.tag);
    _.for(_.pairs(jsonElement.attrs), (attr) => {
      element.setAttribute(attr.key, attr.value);
    });
    if (jsonElement.text) {
      element.appendChild(
        document.createTextNode(jsonElement.text)
      );
    } else {
      _.for(jsonElement.children, (child) => {
        element.appendChild(createElementFromJson(child));
      });
    }
    return element;
  }

  return { onload, get, getAll, appendChildrenFromTemplate, createElementFromJson };
}());
