$(function() {

  $("#hire-date").datepicker({dateFormat: 'yy-mm-dd'});

  $("#language").autocomplete({source: "langs-starting-with"});

  $("#name").blur(function() {
    validateField("name");
  });
  
  $("#language").focus(function() {
    validateField("hire-date");
  });

  $("#language").blur(function() {
    validateField("language");
  });
});

function validateField(id) {

  var value = $("#" + id).val();

  $.getJSON("validate?" + id + "=" + value, function(data){
        
    if(data != null) {

      var errorId = "#" + id + "-error";

      if(data.status == "fail" && data.errors && data.errors[id]) {
        var errorMessage = data.errors[id] [0];
        $(errorId).html(errorMessage);
        $(errorId).show();
      }
      else {
        $(errorId).html("");
        $(errorId).hide();
      }
    }
  });
}
