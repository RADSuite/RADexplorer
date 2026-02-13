function keepQueryAfterSelect(selectId) {
  function apply() {
    var el = document.getElementById(selectId);
    if (!el) return;

    var s = $(el)[0].selectize;
    if (!s || s._keepQueryPatched) return;

    s._keepQueryPatched = true;

    s.on('type', function(str) {
      s._lastQuery = str;
    });

    s.on('item_add', function() {
      var q = s._lastQuery || '';
      s.setTextboxValue(q);
      s.refreshOptions(false);
      s.open();
      s.focus();
    });
  }

  apply();
  $(document).on('shiny:bound', apply);
  $(document).on('shiny:value', apply);
}

$(function() {
  keepQueryAfterSelect('selectTaxa');
  keepQueryAfterSelect('selectGenus');
});