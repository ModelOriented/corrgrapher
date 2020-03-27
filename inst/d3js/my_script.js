function openTab(evt, base_id) {
  // Declare all variables
  var i, tabcontent;
  //console.log(base_id);
  // Get all elements with class="tabcontent" and hide them
  tabcontent = document.getElementsByClassName("cgr_tabcontent");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }

  // Show the current tab, and add an "active" class to the button that opened the tab
  selected_tab_id = base_id + '_' + document.getElementById(base_id + '_' + 'selector').value;
  //console.log(selected_tab_id);
  document.getElementById(selected_tab_id).style.display = "block";
}