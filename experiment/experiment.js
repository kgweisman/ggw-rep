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

/* set up structure of experiment */
var experiment = {
	parts: ["consent", "instructions", "characters", "surveys", "stage", "results", "finished"],
	condition: "none",
	trials: [1,2,3], // temp dummy order
	data: [], // where to store data
	end: function() { // code from long
		showSlide("finished");
		// setTimeout(function() {
		// 	turk.submit(experiment)
		// }, 1500);
	},
	next: function() { // code from long
		if (experiment.trials.length === 0) {
			experiment.end();
		}
		var n = experiment.trials.shift();
		showSlide("stage");
		$("#image-left").attr("src", "images_characters/charlie_dog.png");
		$("#image-right").attr("src", "images_characters/samantha-hill_girl.png");
		var startTime = (new Date()).getTime();
	}
}

/* show consent slide (which allows participant to advance through instructions, characters, and surveys slides) */

showSlide("consent");

$('#surveys button').click(function() { // set condition based on participant's selection of survey
   var id = $(this).attr('id');
   experiment.condition = id;
   console.log(experiment.condition);
});

// 	 fill in stage slide with appropriate question prompt based on survey selected above 

// 	/* fill in stage slide with randomized order of character pairs and randomized left/right position */

// 	/* advance through 78 trials */

// /* when user finishes 78 trials, advance to "demographics" slide */

// showSlide("demographics");

// /* when user clicks "Next" button, advance to "results" slide */

// showSlide("results");

// /* when user clicks "Next" button, advance to "finished" slide */

// showSlide("finished");

// /* submit data via mmturkey */
