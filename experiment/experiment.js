/* set up helper functions from long */

function showSlide(id) { console.log(id);
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
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
	characters[newCharacter.charName] = newCharacter;
};

characters = {};
addCharacter("charlie_dog", "Charlie, Family Dog", "Charlie is a 3-year-old Springer spaniel and a beloved member of the Graham family.");
addCharacter("delores_gleitman_deceased", "Delores Gleitman, Recently Deceased", "Delores Gleitman recently passed away at the age of 65. As you complete the survey, please draw upon your own personal beliefs about people who have passed away.");
addCharacter("fetus", "7-Week Human Fetus", "At 7 weeks, a human fetus is almost half an inch long - roughly the size of a raspberry.");
addCharacter("gerald_schiff_pvs", "Gerald Schiff, in Persistent Vegetative State", "Gerald Schiff has been in a persistent vegetative state (PVS) for the past six months. Although he has severe brain damage - Gerald does not appear to communicate with others or make purposeful movements - his basic bodily functions (such as breathing, sleeping, and circulation) are preserved.");
addCharacter("god", "God", "Many people believe that God is the creator of the universe and the ultimate source of knowledge, power, and love. However, please draw upon your own personal beliefs about God.");
addCharacter("green_frog", "Green Frog", "The Green Frog can be found throughout eastern North America. This classic 'pond frog' is medium-sized and green or bronze in color. Daily life includes seeking out permanent ponds or slow streams with plenty of vegetation.");
addCharacter("kismet_robot", "Kismet, Sociable Robot", "Kismet is part of a new class of 'sociable' robots that can engage people in natural interaction. To do this, Kismet perceives a variety of natural social signals from sound and sight, and delivers his own signals back to the human partner through gaze direction, facial expression, body posture, and vocal babbles.");
addCharacter("nicholas_gannon_baby", "Nicholas Gannon, 5-Month-old", "Nicholas is a five-month-old baby.");
addCharacter("samantha_hill_girl", "Samantha Hill, 5-Year-old", "Samantha is a five-year-old girl who lives with her parents and older sister Jennifer.");
addCharacter("sharon_harvey_woman", "Sharon Harvey, Advertising Executive", "Sharon Harvey, 38, works at an advertising agency in Chicago.");
addCharacter("toby_chimp", "Toby, Wild Chimpanzee", "Toby is a two-year-old wild chimpanzee living at an outdoor laboratory in Uganda.");
addCharacter("todd_billingsley_man", "Todd Billingsley, Accountant", "Todd Billingsley is a thirty-year-old accountant who lives in New York City.");
addCharacter("you", "You", "When you see the mirror, please consider how you, yourself, would compare with the other choice presented.");

/* create the list of all possible pairs (78) */

var pairs = []; 
function makePairs() {
	var list = Object.keys(characters).map(function (key) {return characters[key]});
	for (j = 0; j < 13; j++) {
		for (k = j+1; k < 13; k++) {
			pairs.push([list[j], list[k]]);
		}
	};
};
makePairs();

/* set up list of conditions with wordings */ 

function addCondition(condName, wording) {
	function Condition(condName, wording) {
		this.condName = condName;
		this.wording = wording;
	};
	newCondition = new Condition(condName, wording);
	conditions[newCondition.condName] = newCondition;
};

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
	experiment.info.condition = surveysSlide.order[chosen].condName;
	experiment.info.wording = surveysSlide.order[chosen].wording;
	experiment.next();
	window.scrollTo(0, 0);
});

$('.slide#stage button').click(function() {
	// store selected response
	var response = $(this).attr('id');
	experiment.info.data.response.push(response);
	switch (response) { // omg clean this up
		case "much more left":
			characterMore = experiment.info.data.leftImage[experiment.info.data.leftImage.length-1].charName;
			characterLess = experiment.info.data.rightImage[experiment.info.data.rightImage.length-1].charName;
			experiment.info.charScores[characterMore].push(2);
			experiment.info.charScores[characterLess].push(-2);
			break;
		case "slightly more left":
			characterMore = experiment.info.data.leftImage[experiment.info.data.leftImage.length-1].charName;
			characterLess = experiment.info.data.rightImage[experiment.info.data.rightImage.length-1].charName;
			experiment.info.charScores[characterMore].push(1);
			experiment.info.charScores[characterLess].push(-1);
			break;
		case "both equally":
			characterMore = experiment.info.data.leftImage[experiment.info.data.leftImage.length-1].charName;
			characterLess = experiment.info.data.rightImage[experiment.info.data.rightImage.length-1].charName;
			experiment.info.charScores[characterMore].push(0);
			experiment.info.charScores[characterLess].push(0);
			break;
		case "slightly more right":
			characterMore = experiment.info.data.rightImage[experiment.info.data.rightImage.length-1].charName;
			characterLess = experiment.info.data.leftImage[experiment.info.data.leftImage.length-1].charName;
			experiment.info.charScores[characterMore].push(1);
			experiment.info.charScores[characterLess].push(-1);
			break;
		case "much more right":
			characterMore = experiment.info.data.rightImage[experiment.info.data.rightImage.length-1].charName;
			characterLess = experiment.info.data.leftImage[experiment.info.data.leftImage.length-1].charName;
			experiment.info.charScores[characterMore].push(2);
			experiment.info.charScores[characterLess].push(-2);
			break;
		default: 
			console.log("whoops");
	}

	// show next trial
	this.blur();
	window.scrollTo(0, 0);
	experiment.next();
});

$('.slide#demographics button').click(function() { 
	// record demographic info...
	// text inputs
	demographics.age = $('input#age', '#demographicsForm').val();
	demographics.job = $('input#job', '#demographicsForm').val(); 
	demographics.country = $('input#country', '#demographicsForm').val();
	demographics.children = $('input#children', '#demographicsForm').val();

	// text areas
	demographics.comments = $('.slide#demographics textarea#comments').val();

	// multiple choice radios
	demographics.gender = $('input[name=gender]:checked', '#demographicsForm').val();
	demographics.education = $('input[name=education]:checked', '#demographicsForm').val();
	demographics.englishNative = $('input[name=englishNative]:checked', '#demographicsForm').val();
	demographics.maritalStatus = $('input[name=maritalStatus]:checked', '#demographicsForm').val();
	demographics.vegetarian = $('input[name=vegetarian]:checked', '#demographicsForm').val();
	demographics.studyMoralPhil = $('input[name=studyMoralPhil]:checked', '#demographicsForm').val();
	demographics.politicalIdeology = $('input[name=politicalIdeology]:checked', '#demographicsForm').val();
	demographics.beliefGod = $('input[name=beliefGod]:checked', '#demographicsForm').val();
	demographics.beliefTradition = $('input[name=beliefTradition]:checked', '#demographicsForm').val();
	demographics.beliefAfterlife = $('input[name=beliefAfterlife]:checked', '#demographicsForm').val();
	demographics.beliefLeader = $('input[name=beliefLeader]:checked', '#demographicsForm').val();
	demographics.beliefRules = $('input[name=beliefRules]:checked', '#demographicsForm').val();

	// multiple answer checkboxes
	$('input[name=ethnicity]:checked', '#demographicsForm').each(function() {
		demographics.ethnicity.push($(this).val());
	});
	$('input[name=religionChild]:checked', '#demographicsForm').each(function() {
		demographics.religionChild.push($(this).val());
	});
	$('input[name=religionNow]:checked', '#demographicsForm').each(function() {
		demographics.religionNow.push($(this).val());
	});

	// send demographic info to experiment object
	experiment.info.demographics = demographics;

	// set up results page
	resultsSlide.calculateMeans(); 
	resultsSlide.orderCharacters(); 
	resultsSlide.showOrder(); 
	window.scrollTo(0, 0);

	showSlide("results");
});

$('.slide#results button').click(function() {
	window.scrollTo(0, 0);
	turk.submit(experiment); // is it ok to wait this long to submit? what if people quit when they see the demographics slide, or don't care about looking at their results?
	setTimeout(function() {
		$('.slide#finished p').text("You're finished - thanks for participating! Submitting to Mechanical Turk... done.");
	}, 2000);

	// showSlide("finished");
});

/* set up how to display characters slide */

var charactersSlide = {
	list: Object.keys(characters).map(function (key) {return characters[key]}),
	order: [],
	makeOrder: function() {
		for (i = 0; i < 13; i++) {
			this.order.push(randomElementNR(this.list))
		}
	},
	showOrder: function() {
		for (i = 0; i < this.order.length; i++) {
			var charNum = i.toString();
			$("h2#character"+charNum).text(charactersSlide.order[i].charTitle);
			$("img#character"+charNum).attr("src", charactersSlide.order[i].imageSource);
			$("p#character"+charNum).text(charactersSlide.order[i].charDescrip);
		};
		experiment.info.charIntroOrder = this.order; // store order of introduction of conditions in experiment object
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
		experiment.info.condIntroOrder = this.order; // store order of introduction of conditions in experiment object
	}
}

/* set up how to display stage slide (experiment trials) */

var experiment = {
	trials: pairs,
	info: { // where to store data
		charIntroOrder: [],
		condIntroOrder: [],
		condition: [],
		wording: [],
		data: {
			leftImage: [],
			rightImage: [],
			response: []
			// , rt: []
		},
		charScores: {
			charlie_dog: [],
			delores_gleitman_deceased: [],
			fetus: [],
			gerald_schiff_pvs: [],
			god: [],
			green_frog: [],
			kismet_robot: [],
			nicholas_gannon_baby: [],
			samantha_hill_girl: [],
			sharon_harvey_woman: [],
			toby_chimp: [],
			todd_billingsley_man: [],
			you: []
		}
	}, 
	end: function() { 
		showSlide("demographics");
	},
	next: function() { // code from long
		if (this.trials.length === 0) {
			experiment.end();
		} else { 
			// create bucket for selecting left vs. right position of images
			var sideBucket = [0,1]; 

			// store data about which pair and positions of characters for this trial
			var trialPair = randomElementNR(this.trials);

			this.info.data.leftImage.push(trialPair[randomElementNR(sideBucket)]);
			this.info.data.rightImage.push(trialPair[sideBucket]);

			// display trial number (temporary for development?)
			var trialNum = this.info.data.leftImage.length.toString();
			var percentComplete = Math.round((this.info.data.leftImage.length-1)/78 * 100);
			$("#trial-num").text("trial "+trialNum+" of 78: "+percentComplete+"% complete");

			// set text and images for this trial
			$(".slide#stage #question").text("Which character do you think is more capable of "+this.info.wording+"?");
			$("#stage #image-left").attr("src", this.info.data.leftImage[trialNum-1].imageSource);
			$("#stage #image-right").attr("src", this.info.data.rightImage[trialNum-1].imageSource);
			$("#stage #text-left").text(this.info.data.leftImage[trialNum-1].charTitle);
			$("#stage #text-right").text(this.info.data.rightImage[trialNum-1].charTitle);

			// show trial
			showSlide("stage");

			// // start timing 
			// var startTime = (new Date()).getTime();
			// var keyPressHandler = function(event) {
			// 	var endTime = (new Date()).getTime();
			// 	var rt = endTime - startTime;
			// 	experiment.info.data.rt.push(rt);
			// 	console.log(rt);
			// }

			// $('.slide#stage button').one("click", keyPressHandler);
		}
	}
};

/* gather data from demographics survey */

var demographics = {
	age: [],
	gender: [],
	job: [],
	education: [],
	ethnicity: [],
	religionChild: [],
	religionNow: [],
	country: [],
	englishNative: [],
	maritalStatus: [],
	children: [],
	vegetarian: [],
	studyMoralPhil: [],
	politicalIdeology: [],
	beliefGod: [],
	beliefTradition: [],
	beliefAfterlife: [],
	beliefLeader: [],
	beliefRules: [],
	comments: []
}

/* set up how to display results */

var resultsSlide = {
	list: characters,
	charScores: experiment.info.charScores,
	charMeans: {
		charlie_dog: [],
		delores_gleitman_deceased: [],
		fetus: [],
		gerald_schiff_pvs: [],
		god: [],
		green_frog: [],
		kismet_robot: [],
		nicholas_gannon_baby: [],
		samantha_hill_girl: [],
		sharon_harvey_woman: [],
		toby_chimp: [],
		todd_billingsley_man: [],
		you: []
	},
	charSorted: [],
	calculateMeans: function() {
		for (i in this.charMeans) {
			array = this.charScores[i];
			for (j = 0; j < array.length; j++) {
				var total =+ array[j];
			}
			mean = total/array.length
			this.charMeans[i] = mean;
		}
	},
	orderCharacters: function() {
		sortedCharacters = [];
		for (i in this.charMeans) {
			sortedCharacters.push([i, this.charMeans[i]]);
		}		
		sortedCharacters = sortedCharacters.sort(function(a, b) {return b[1] - a[1]});
		this.charSorted = sortedCharacters;
	},
	showOrder: function() {
		for (i = 0; i < this.charSorted.length; i++) {
			var charNum = i+1;
			var charName = this.charSorted[i][0];
			$("p#rankingIntro").text("Here's how you ranked these characters, from most to least capable of "+experiment.info.wording+":");
			$("p#rank"+charNum).text(characters[charName].charTitle);
			$("img#rank"+charNum).attr("src", characters[charName].imageSource);
		}
	}
}

/* show consent slide (which then advances through all remaining slides) */

showSlide("consent");