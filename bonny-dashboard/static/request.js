"use strict";
const Request = (function(){
  function ajaxGet(url, success, failure) {
    var request = new XMLHttpRequest();
    request.open('GET', url, true);
    request.onreadystatechange = function () {
      if (request.readyState === XMLHttpRequest.DONE) {
        if (request.status === 200) {
          success(request.responseText)
        } else {
          failure(request.status, request.statusText, request.responseText);
        }
      }
    }
    return request.send();
  }

  function formatParams(params) {
    return "?" + _.pairs(params)
      .map(param => encodeURIComponent(param.key) + "=" + encodeURIComponent(param.value))
      .join("&");
  }

  function get(url, params) {
    return new Promise((resolve, reject) => {
      ajaxGet(
        url + formatParams(params),
        (response) => resolve(JSON.parse(response)),
        (code, status, response) => reject({ code, status, response })
      );
    });
  }

  return { get };
}());
