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

function addCharacter(charName, charTitle, charDescrip) {
	function Character(charName, charTitle, charDescrip) {
		this.charName = charName;
		this.charTitle = charTitle;
		this.imageSource = "images_characters/"+charName+".png";
		this.charDescrip = charDescrip;
	};
	newCharacter = new Character(charName, charTitle, charDescrip);
	characters.push(newCharacter);
}

var characters = [];
addCharacter("charlie_dog", "Charlie, Family Dog", "Charlie is a 3-year-old Springer spaniel and a beloved member of the Graham family.");
addCharacter("delores-gleitman_deceased", "Delores Gleitman, Recently Deceased", "Delores Gleitman recently passed away at the age of 65. As you complete the survey, please draw upon your own personal beliefs about people who have passed away.");
addCharacter("fetus", "7-Week Human Fetus", "At 7 weeks, a human fetus is almost half an inch long - roughly the size of a raspberry.");
addCharacter("gerald-schiff_pvs", "Gerald Schiff, in Persistent Vegetative State", "Gerald Schiff has been in a persistent vegetative state (PVS) for the past six months. Although he has severe brain damage - Gerald does not appear to communicate with others or make purposeful movements - his basic bodily functions (such as breathing, sleeping, and circulation) are preserved.");
addCharacter("god", "God", "Many people believe that God is the creator of the universe and the ultimate source of knowledge, power, and love. However, please draw upon your own personal beliefs about God.");
addCharacter("green-frog", "Green Frog", "The Green Frog can be found throughout eastern North America. This classic 'pond frog' is medium-sized and green or bronze in color. Daily life includes seeking out permanent ponds or slow streams with plenty of vegetation.");
addCharacter("kismet_robot", "Kismet, Sociable Robot", "Kismet is part of a new class of 'sociable' robots that can engage people in natural interaction. To do this, Kismet perceives a variety of natural social signals from sound and sight, and delivers his own signals back to the human partner through gaze direction, facial expression, body posture, and vocal babbles.");
addCharacter("nicholas-gannon_baby", "Nicholas Gannon, 5-Month-old", "Nicholas is a five-month-old baby.");
addCharacter("samantha-hill_girl", "Samantha Hill, 5-Year-old", "Samantha is a five-year-old girl who lives with her parents and older sister Jennifer.");
addCharacter("sharon-harvey_woman", "Sharon Harvey, Advertising Executive", "Sharon Harvey, 38, works at an advertising agency in Chicago.");
addCharacter("toby_chimp", "Toby, Wild Chimpanzee", "Toby is a two-year-old wild chimpanzee living at an outdoor laboratory in Uganda.");
addCharacter("todd-billingsley_man", "Todd Billingsley, Accountant", "Todd Billingsley is a thirty-year-old accountant who lives in New York City.");
addCharacter("you", "You", "When you see the mirror, please consider how you, yourself, would compare with the other choice presented.");

/* create the list of all possible pairs (78) */

var pairs = [] 

for (j = 0; j < 13; j++) {
	for (k = j+1; k < 13; k++) {
		pairs.push([characters[j], characters[k]]);
	}
}

/* set up list of conditions with wordings */ 

function addCondition(condName, wording) {
	function Condition(condName, wording) {
		this.condName = condName;
		this.wording = wording;
	};
	newCondition = new Condition(condName, wording);
	conditions[newCondition.condName] = newCondition;
}

conditions = {};
addCondition("communication", "conveying thoughts or feelings to others");
addCondition("consciousness", "having experiences and being aware of things");
addCondition("desire", "longing or hoping for things");
addCondition("embarrassment", "experiencing embarrassment");
addCondition("emotion-recog", "understanding how others are feeling");
addCondition("fear", "feeling afraid or fearful");
addCondition("hunger", "feeling hungry");
addCondition("joy", "experiencing joy");
addCondition("memory", "remembering things");
addCondition("morality", "telling right from wrong and trying to do the right thing");
addCondition("pain", "experiencing physical or emotional pain");
addCondition("personality", "having personality traits that make it unique from others");
addCondition("planning", "making plans and working toward goal");
addCondition("pleasure", "experiencing physical or emotional pleasure");
addCondition("pride", "experiencing pride");
addCondition("rage", "experiencing violent or uncontrolled anger");
addCondition("self", "exercising self-restraint over desires, emotions, or impulses");
addCondition("thought", "thinking");

$('#surveys button').click(function() {
	// var chosenCondition = $(this).attr('id');
	var chosen = $(this).attr('id');
	experiment.data.condition = chosen;
	experiment.data.wording = conditions[chosen.toString()].wording;
	experiment.next();
})

$('#stage button').click(function() {
	var response = $(this).attr('id');
	experiment.data.response.push(response);
	this.blur();
	experiment.next();
})

/* set up how to display experiment slides */

var experiment = {
	trials: pairs,
	data: {
		condition: [],
		wording: [],
		leftImage: [],
		rightImage: [],
		response: []
	}, // where to store data
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
			// create bucket for selecting left vs. right position of images
			var sideBucket = [0,1]; 

			// store data about which pair and positions of characters for this trial
			var trialPair = randomElementNR(this.trials);

			this.data.leftImage.push(trialPair[randomElementNR(sideBucket)]);
			this.data.rightImage.push(trialPair[sideBucket]);

			// display trial number (temporary for development?)
			var trialNum = this.data.leftImage.length.toString();
			var percentComplete = Math.round((this.data.leftImage.length-1)/78 * 100);
			$("#trial-num").text("trial "+trialNum+" of 78: "+percentComplete+"% complete");

			// set text and images for this trial
			$("#question").text("Which character do you think is more capable of "+this.data.wording+"?");
			$("#image-left").attr("src", this.data.leftImage[trialNum-1].imageSource);
			$("#image-right").attr("src", this.data.rightImage[trialNum-1].imageSource);
			$("#text-left").text(this.data.leftImage[trialNum-1].charTitle);
			$("#text-right").text(this.data.rightImage[trialNum-1].charTitle);

			// show trial
			showSlide("stage");

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
