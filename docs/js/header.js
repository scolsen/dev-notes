//header.js
function sizer(){
	window.addEventListener('scroll', function(e){
		var distance = window.pageYOffset || document.documentElement.scrollTop,
			shrink  = 100,
			target = document.querySelector("#site-header");
		if (distance > shrink){
			target.className = "site-header-mini";
		} else {
			if(target.className == "site-header-mini"){
				target.className = "site-header";
			}
		}
	});
}

window.onload = sizer();
