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

/* set up list of characters with image sources */ 

function Character(charName) {
	this.charName = charName;
	this.imageSource = "images_characters/"+charName+".png";
}

var charlie = new Character("charlie_dog");
var delores = new Character("delores-gleitman_deceased");
var fetus = new Character("fetus");
var gerald = new Character("gerald-schiff_pvs");
var god = new Character("god");
var greenfrog = new Character("green-frog");
var kismet = new Character("kismet_robot");
var nicholas = new Character("nicholas-gannon_baby");
var samantha = new Character("samantha-hill_girl");
var sharon = new Character("sharon-harvey_woman");
var toby = new Character("toby_chimp");
var todd = new Character("todd-billingsley_man");
var you = new Character("you");
var characters = [charlie, delores, fetus, gerald, 
				god, greenfrog, kismet, nicholas,
				samantha, sharon, toby, todd, you]; // how bad is it to define these all as global vars?

function Pair(character1, character2) {
	this.character1 = character1;
	this.character2 = character2;
}

var pairs = [] // create the list of all possible pairs (78)

for (j = 0; j < characters.length; j++) {
	for (k = j+1; k < characters.length; k++) {
		pairs[BROKEN] = new Pair(characters[j], characters[k]);
	}
}

console.log(pairs);

/* set up how to display experiment slides */

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
		this.data.leftImage = characters[randomInteger(characters.length)];
		this.data.rightImage = characters[randomInteger(characters.length)];
		$("#image-left").attr("src", this.data.leftImage.imageSource);
		$("#image-right").attr("src", this.data.rightImage.imageSource);
		var startTime = (new Date()).getTime();
		console.log(this.data.leftImage);
		console.log(this.data.rightImage);
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
