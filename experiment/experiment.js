/* set up helper functions from long */

function showSlide(id) {
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
	console.log(id);
};

function randomInteger(n) { // get a random integer < n
	return Math.floor(Math.random()*n); 
}

function randomElementNR(bucket) { // select random element of array without replacement
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
addCondition("Communication", "conveying thoughts or feelings to others");
addCondition("Consciousness", "having experiences and being aware of things");
addCondition("Desire", "longing or hoping for things");
addCondition("Embarrassment", "experiencing embarrassment");
addCondition("Emotion Recognition", "understanding how others are feeling");
addCondition("Fear", "feeling afraid or fearful");
addCondition("Hunger", "feeling hungry");
addCondition("Joy", "experiencing joy");
addCondition("Memory", "remembering things");
addCondition("Morality", "telling right from wrong and trying to do the right thing");
addCondition("Pain", "experiencing physical or emotional pain");
addCondition("Personality", "having personality traits that make it unique from others");
addCondition("Planning", "making plans and working toward goal");
addCondition("Pleasure", "experiencing physical or emotional pleasure");
addCondition("Pride", "experiencing pride");
addCondition("Rage", "experiencing violent or uncontrolled anger");
addCondition("Self", "exercising self-restraint over desires, emotions, or impulses");
addCondition("Thought", "thinking");

/* set up button behaviors */

$('.slide#characters button').click(function() {
	window.scrollTo(0, 0);
});

$('.slide#surveys button').click(function() { // select condition
	var chosen = $(this).attr('id');
	experiment.data.condition = surveysSlide.order[chosen].condName;
	experiment.data.wording = surveysSlide.order[chosen].wording;
	experiment.next();
	window.scrollTo(0, 0);
});

$('.slide#stage button').click(function() { // store response
	var response = $(this).attr('id');
	experiment.data.response.push(response);
	this.blur();
	experiment.next();
	window.scrollTo(0, 0);
});

$('.slide#results button').click(function() {
	window.scrollTo(0, 0);
});

$('.slide#finished button').click(function() {
	window.scrollTo(0, 0);
});

/* set up how to display characters slide */

var charactersSlide = {
	list: characters,
	order: [],
	makeOrder: function() {
		for (i = 0; i < 13; i++) {
			this.order.push(randomElementNR(this.list))
		}
	},
	showOrder: function() {
		for (i = 0; i < this.order.length; i++) {
			var charNum = i+1;
			$("h2#character"+charNum).text(charactersSlide.order[i].charTitle);
			$("img#character"+charNum).attr("src", charactersSlide.order[i].imageSource);
			$("p#character"+charNum).text(charactersSlide.order[i].charDescrip);
		}
		experiment.data.charIntroOrder = this.order; // store order of introduction of characters in experiment object
	}
}

/* set up how to display surveys slide */

var surveysSlide = {
	list: Object.keys(conditions).map(function (key) {return conditions[key]}),
	order: [],
	makeOrder: function() {
		for (i = 0; i < 18; i++) {
			this.order.push(randomElementNR(this.list))
		}
	},
	showOrder: function() {
		for (i = 0; i < this.order.length; i++) {
			var condNum = i.toString();
			$("#surveys h2#"+condNum).text(surveysSlide.order[i].condName);
			$("#surveys p#"+condNum).text("This survey asks you to judge which character is more capable of "+surveysSlide.order[i].wording+".");
			$("#surveys button#"+condNum).text("Select "+surveysSlide.order[i].condName+" Survey");
		}
		experiment.data.condIntroOrder = this.order; // store order of introduction of conditions in experiment object
	}
}

/* set up how to display stage slide (experiment trials) */

var experiment = {
	trials: pairs,
	data: {
		charIntroOrder: [],
		condIntroOrder: [],
		condition: [],
		wording: [],
		leftImage: [],
		rightImage: [],
		response: [],
		RT: []
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
			$(".slide#stage #question").text("Which character do you think is more capable of "+this.data.wording+"?");
			$("#stage #image-left").attr("src", this.data.leftImage[trialNum-1].imageSource);
			$("#stage #image-right").attr("src", this.data.rightImage[trialNum-1].imageSource);
			$("#stage #text-left").text(this.data.leftImage[trialNum-1].charTitle);
			$("#stage #text-right").text(this.data.rightImage[trialNum-1].charTitle);

			// show trial
			showSlide("stage");

			// var startTime = (new Date()).getTime(); // do I really want RT?
		}
	}
}

/* show consent slide (which then advances through all remaining slides) */

showSlide("consent");

/* TO DO: submit data via mmturkey */