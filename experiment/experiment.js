/* set up helper functions from long */

function showSlide(id) {
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
};

function randomInteger(n) { 
	return Math.floor(Math.random()*n); // get a random integer < n
}

function randomElement(array) {
	return array[randomInteger(array.length)]; // get a random element out of an array
}

/* show consent slide */

showSlide("consent");

/* when user clicks "Next" button, advance to "instructions" slide */

/* when user clicks "Next" button, advance to "characters" slide */

/* when user clicks "Next" button, advance to "surveys" slide */

/* when user clicks "Start ___ Survey" button, advance to stage slide */

	/* fill in stage slide with appropriate question prompt based on survey selected above */

	/* fill in stage slide with randomized order of character pairs and randomized left/right position */

	/* advance through 78 trials */

/* when user finishes 78 trials, advance to "demographics" slide */

/* when user clicks "Next" button, advance to "results" slide */

/* when user clicks "Next" button, advance to "finished" slide */

/* submit data via mmturkey */
