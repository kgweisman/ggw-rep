/* set up helper functions from long */

function showSlide(id) {
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
};

function randomInteger(n) { // get a random integer < n
	return Math.floor(Math.random()*n); 
}

// function randomElement(array) { // get a random element out of an array
// 	return array[randomInteger(array.length)]; 
// }

function randomElementNR(bucket) { // select without replacement
	var randomIndex = randomInteger(bucket.length);
	return bucket.splice(randomIndex, 1)[0];
}

/* set up list of characters with titles, descriptions, and image sources */ 

function Character(charName, charTitle, charDescrip) {
	this.charName = charName;
	this.charTitle = charTitle;
	this.imageSource = "images_characters/"+charName+".png";
	this.charDescrip = charDescrip;
}

var charlie = new Character("charlie_dog", "Charlie, Family Dog", "Charlie is a 3-year-old Springer spaniel and a beloved member of the Graham family.");
var delores = new Character("delores-gleitman_deceased", "Delores Gleitman, Recently Deceased", "Delores Gleitman recently passed away at the age of 65. As you complete the survey, please draw upon your own personal beliefs about people who have passed away.");
var fetus = new Character("fetus", "7-Week Human Fetus", "At 7 weeks, a human fetus is almost half an inch long - roughly the size of a raspberry.");
var gerald = new Character("gerald-schiff_pvs", "Gerald Schiff, in Persistent Vegetative State", "Gerald Schiff has been in a persistent vegetative state (PVS) for the past six months. Although he has severe brain damage - Gerald does not appear to communicate with others or make purposeful movements - his basic bodily functions (such as breathing, sleeping, and circulation) are preserved.");
var god = new Character("god", "God", "Many people believe that God is the creator of the universe and the ultimate source of knowledge, power, and love. However, please draw upon your own personal beliefs about God.");
var greenfrog = new Character("green-frog", "Green Frog", "The Green Frog can be found throughout eastern North America. This classic 'pond frog' is medium-sized and green or bronze in color. Daily life includes seeking out permanent ponds or slow streams with plenty of vegetation.");
var kismet = new Character("kismet_robot", "Kismet, Sociable Robot", "Kismet is part of a new class of 'sociable' robots that can engage people in natural interaction. To do this, Kismet perceives a variety of natural social signals from sound and sight, and delivers his own signals back to the human partner through gaze direction, facial expression, body posture, and vocal babbles.");
var nicholas = new Character("nicholas-gannon_baby", "Nicholas Gannon, 5-Month-old", "Nicholas is a five-month-old baby.");
var samantha = new Character("samantha-hill_girl", "Samantha Hill, 5-Year-old", "Samantha is a five-year-old girl who lives with her parents and older sister Jennifer.");
var sharon = new Character("sharon-harvey_woman", "Sharon Harvey, Advertising Executive", "Sharon Harvey, 38, works at an advertising agency in Chicago.");
var toby = new Character("toby_chimp", "Toby, Wild Chimpanzee", "Toby is a two-year-old wild chimpanzee living at an outdoor laboratory in Uganda.");
var todd = new Character("todd-billingsley_man", "Todd Billingsley, Accountant", "Todd Billingsley is a thirty-year-old accountant who lives in New York City.");
var you = new Character("you", "You", "When you see the mirror, please consider how you, yourself, would compare with the other choice presented.");

var characters = [charlie, delores, fetus, gerald, 
				god, greenfrog, kismet, nicholas,
				samantha, sharon, toby, todd, you]; // how bad is it to define these all as global vars?

var pairs = [] // create the list of all possible pairs (78)

for (j = 0; j < characters.length; j++) {
	for (k = j+1; k < characters.length; k++) {
		pairs.push([characters[j], characters[k]]);
	}
}

/* set up list of conditions with wordings */ 

function Condition(condName, wording) {
	this.condName = condName;
	this.wording = wording;
}

var communication = new Condition("communication", "conveying thoughts or feelings to others")
var consciousness = new Condition("consciousness", "having experiences and being aware of things")
var desire = new Condition("desire", "longing or hoping for things")
var embarrassment = new Condition("embarrassment", "experiencing embarrassment")
var emotionRecog = new Condition("emotion-recog", "understanding how others are feeling")
var fear = new Condition("fear", "feeling afraid or fearful")
var hunger = new Condition("hunger", "feeling hungry")
var joy = new Condition("joy", "experiencing joy")
var memory = new Condition("memory", "remembering things")
var morality = new Condition("morality", "telling right from wrong and trying to do the right thing")
var pain = new Condition("pain", "experiencing physical or emotional pain")
var personality = new Condition("personality", "having personality traits that make it unique from others")
var planning = new Condition("planning", "making plans and working toward goal")
var pleasure = new Condition("pleasure", "experiencing physical or emotional pleasure")
var pride = new Condition("pride", "experiencing pride")
var rage = new Condition("rage", "experiencing violent or uncontrolled anger")
var self = new Condition("self", "exercising self-restraint over desires, emotions, or impulses")
var thought = new Condition("thought", "thinking")

var conditions = [communication, consciousness, desire, embarrassment, emotionRecog, fear, hunger, joy, memory, morality, pain, personality, planning, pleasure, pride, rage, self, thought];

$('#surveys button').click(function() {
	// var chosenCondition = $(this).attr('id');
	experiment.data.condition = $(this).attr('id');
})

/* set up how to display experiment slides */

var experiment = {
	trials: pairs,
	data: [], // where to store data
	end: function() { // code from long
		showSlide("demographics");
		// setTimeout(function() {
		// 	turk.submit(experiment)
		// }, 1500);
	},
	next: function() { // code from long
		if (this.trials.length === 0) {
			experiment.end();
		} else {
			var sideBucket = [0,1]; // bucket for selecting left vs. right position of images
			this.data.pair = randomElementNR(this.trials);
			this.data.leftImage = this.data.pair[randomElementNR(sideBucket)];
			this.data.rightImage = this.data.pair[sideBucket];
			$("#question").text("Which character do you think is more capable of "+this.data.condition+"?")
			$("#image-left").attr("src", this.data.leftImage.imageSource);
			$("#image-right").attr("src", this.data.rightImage.imageSource);
			$("#text-left").text(this.data.leftImage.charTitle);
			$("#text-right").text(this.data.rightImage.charTitle);
			showSlide("stage");
			console.log(this.data.condition);
			// var startTime = (new Date()).getTime(); // do I really want RT?
		}
	}
}

/* show consent slide (which allows participant to advance through instructions, characters, and surveys slides) */

showSlide("consent");

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
