function(properties) {
  var obj = this.body.container.parentNode;
  while(!/^cgr_content_[0-9]{6}$/.test(obj.id)){
    obj = obj.parentElement;
  }
  console.log('Triggered!');
  var base_id = obj.id;
  
  var i, tabcontent;
  tabcontent = document.getElementsByClassName('cgr_tabcontent');
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = 'none';
  }
  
  selected_node_label = this.body.data.nodes.get(properties.nodes[0]).label;
  obj = document.getElementById(base_id + '_' + selected_node_label);
  obj.style.display = 'block';
  window.dispatchEvent(new Event('resize'));
}