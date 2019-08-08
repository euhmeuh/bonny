"use strict";
const Datagrid = (function(){
  const filterOperators = {
    eq: "is",
    neq: "is not",
    in: "contains",
    nin: "does not contain",
    starts: "begins with",
    ends: "ends with"
  };

  function Datagrid(id) {
    this.dom = document.getElementById(id);
    this.dom.style.display = "none";
    this.table = this._createTable();
    this.init({
      resource: this.dom.dataset.resource,
      onError: this.dom.dataset["on-error"],
      columns: Array.from(this.dom.getElementsByTagName("bonny-column")),
      headers: Array.from(this.dom.getElementsByTagName("bonny-header")),
      footers: Array.from(this.dom.getElementsByTagName("bonny-footer")),
      loader: this.dom.getElementsByTagName("bonny-loader")[0],
      placeholder: this.dom.getElementsByTagName("bonny-placeholder")[0],
    });
  }

  Datagrid.prototype._getFilterableColumns = function() {
    return this.columns.filter(col => col.dataset.filterable !== undefined);
  }

  Datagrid.prototype._createTable = function() {
    const table = Page.createElementFromJson({
      tag: "table",
      attrs: { class: "bonny-datagrid " + this.dom.getAttribute("class") },
      children: [ { tag: "thead" }, { tag: "tbody" }, { tag: "tfoot" } ]
    });
    this.dom.parentNode.insertBefore(table, this.dom);
    return table;
  }

  Datagrid.prototype._createFilterAdder = function() {
    return Page.createElementFromJson({
      tag: "form",
      attrs: { class: "filter-adder" },
      children: [
        {
          tag: "select",
          attrs: { name: "new-filter-column" },
          children: this._getFilterableColumns().map(column => ({
            tag: "option",
            attrs: { value: column.dataset.value },
            text: column.dataset.name
          }))
        },
        {
          tag: "select",
          attrs: { name: "new-filter-operator" },
          children: _.pairs(filterOperators).map(operator => ({
            tag: "option",
            attrs: { value: operator.key },
            text: operator.value
          }))
        },
        { tag: "input", attrs: { name: "new-filter-value", type: "text" } },
        { tag: "button", text: "Add" },
      ]
    });
  }

  Datagrid.prototype._createPageSizeSelector = function() {
    return Page.createElementFromJson({
      tag: "select",
      attrs: { name: "page-size" },
      children: _.parseArray(this.dom.dataset.pageSizes).map(size => ({
        tag: "option", attrs: { value: size }, text: size
      }))
    });
  }

  Datagrid.prototype._createPagination = function() {
    return Page.createElementFromJson({
      tag: "div",
      attrs: { class: "pagination" },
      children: [
        { tag: "div", attrs: { class: "pagination-link pagination-first" }, text: "First page" },
        { tag: "div", attrs: { class: "pagination-link pagination-previous" }, text: "Previous page" },
        { tag: "div", attrs: { class: "pagination-link" }, text: "1" },
        { tag: "div", attrs: { class: "pagination-link pagination-current" }, text: "2" },
        { tag: "div", attrs: { class: "pagination-link" }, text: "3" },
        { tag: "div", attrs: { class: "pagination-link pagination-next" }, text: "Next page" },
        { tag: "div", attrs: { class: "pagination-link pagination-last" }, text: "Last page" },
      ]
    });
  }

  Datagrid.prototype._createFilterList = function() {
    return Page.createElementFromJson({
      tag: "div", attrs: { class: "filter-list" },
      children: [
        {
          tag: "span",
          attrs: { class: "filter" },
          children: [
            { tag: "span", text: "Name contains \"ra\"" },
            { tag: "span", attrs: { class: "filter-close" } }
          ]
        },
        {
          tag: "span",
          attrs: { class: "filter" },
          children: [
            { tag: "span", text: "Status is not wandering" },
            { tag: "span", attrs: { class: "filter-close" } }
          ]
        }
      ]
    })
  }

  Datagrid.prototype._createResultCount = function() {
    return Page.createElementFromJson({
      tag: "span", text: "42"
    });
  }

  Datagrid.prototype._addRow = function(row, sectionTag) {
    const section = this.table.getElementsByTagName(sectionTag)[0];
    section.appendChild(row);
  }

  Datagrid.prototype._resetRows = function(sectionTag) {
    const section = this.table.getElementsByTagName(sectionTag)[0];
    while (section.firstChild) {
      section.removeChild(section.firstChild);
    }
  }

  Datagrid.prototype._renderSimpleRow = function(template) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.setAttribute("colspan", this.colSize);
    Page.appendChildrenFromTemplate(td, template);
    tr.appendChild(td);
    return tr;
  }

  Datagrid.prototype._renderHeaderRow = function() {
    const tr = document.createElement("tr");
    _.for(this.columns, (column) => {
      const th = document.createElement("th");
      if (column.dataset.sortable !== undefined) {
        th.setAttribute("class", "sortable");
        th.setAttribute("data-direction", "asc");
        th.addEventListener("click", event => this._handleSortClick(column, event.target));
      }
      const name = document.createElement("span");
      name.appendChild(document.createTextNode(column.dataset.name));
      th.appendChild(name);
      tr.appendChild(th);
    });
    return tr;
  }

  Datagrid.prototype._renderResourceRow = function(res) {
    const tr = document.createElement("tr");
    _.for(this.columns, (column) => {
      tr.appendChild(this._renderColumn(column, res));
    });
    return tr;
  }

  Datagrid.prototype._renderColumn = function(column, res) {
    const td = document.createElement("td");
    if (column.hasChildNodes()) {
      Page.appendChildrenFromTemplate(td, column, res);
    } else {
      td.appendChild(document.createTextNode(_.get(res, column.dataset.value, "")))
    }
    return td;
  }

  Datagrid.prototype._renderComponents = function(tagName, renderer) {
    const elements = this.dom.getElementsByTagName(tagName);
    _.for(Array.from(elements), element => {
      element.parentNode.insertBefore(renderer.call(this), element);
      element.parentNode.removeChild(element);
    })
  }

  Datagrid.prototype._showLoader = function() {
    this._resetRows("tbody");
    this._addRow(this._renderSimpleRow(this.loader), "tbody");
  };

  Datagrid.prototype._showPlaceholder = function() {
    this._resetRows("tbody");
    this._addRow(this._renderSimpleRow(this.placeholder), "tbody");
  };

  Datagrid.prototype._handleSortClick = function(column, element) {
    const direction = element.dataset.direction;
    element.dataset.direction = _.toggle(direction, ["asc", "desc"]);
    this.sortBy(column, direction);
  };

  Datagrid.prototype.init = function(config) {
    this.resource = _.get(config, "resource");
    this.onError = _.get(config, "onError", (error) => console.log(error));
    this.columns = _.get(config, "columns", []);
    this.headers = _.get(config, "headers", []);
    this.footers = _.get(config, "footers", []);
    this.loader = _.get(config, "loader", document.createTextNode("Loading content..."));
    this.placeholder = _.get(config, "placeholder", document.createTextNode("No results."));
    this.colSize = this.columns.length;

    this._renderComponents("bonny-filter-adder", this._createFilterAdder);
    this._renderComponents("bonny-filter-list", this._createFilterList);
    this._renderComponents("bonny-result-count", this._createResultCount);
    this._renderComponents("bonny-page-size-selector", this._createPageSizeSelector);
    this._renderComponents("bonny-pagination", this._createPagination);

    _.for(this.headers, (header) => {
      this._addRow(this._renderSimpleRow(header), "thead");
    });
    this._addRow(this._renderHeaderRow(), "thead");

    _.for(this.footers, (footer) => {
      this._addRow(this._renderSimpleRow(footer), "tfoot");
    });

    this.refresh();
  }

  Datagrid.prototype.refresh = function(config) {
    if (!this.resource) {
      this._showPlaceholder();
      return;
    }

    this._showLoader();

    Request.get(this.resource, config)
      .then((resources) => {
        this._resetRows("tbody");
        if (resources) {
          _.for(resources, (res) => {
            this._addRow(this._renderResourceRow(res), "tbody");
          });
        } else {
          this._addRow(this._renderSimpleRow(this.placeholder), "tbody");
        }
      })
      .catch(error => {
        this.onError(error);
        this._showPlaceholder();
      });
  };

  Datagrid.prototype.addFilter = function() {

  }

  Datagrid.prototype.removeFilter = function() {

  }

  Datagrid.prototype.sortBy = function(column, direction) {
    this.refresh({
      sort: column.dataset.value,
      direction
    });
  }

  return Datagrid;
}());
