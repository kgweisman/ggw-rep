/* set up helper functions */

// define function to show a slide
function showSlide(id) { 
	// console.log(id); // show which slide is being presented
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
};

// define function to get a random integer < n
function randomInteger(n) { 
	return Math.floor(Math.random()*n); 
}

// define function for random selection without replacement
function randomElementNR(bucket) { 
	var randomIndex = randomInteger(bucket.length);
	return bucket.splice(randomIndex, 1)[0];
}

// define function for preloading images (code from long)
function preload(images, onLoadedOne, onLoadedAll) {
	showSlide("preload");

	var remainingImages = images.slice();
	var finished = false;

	// set delayInterval to 800 for testing to see that everything actually loads
	// for real use, set to 0 
	var loadDelayInterval = 0;

	var worker = function() {
	  if (remainingImages.length == 0) {
	    if (!finished) {
	      finished = true;
	      setTimeout(onLoadedAll, loadDelayInterval);
	    }
	} else {

	    var src = remainingImages.shift(); 
	    
	    var image = new Image();
	    image.onload = function() {
	        onLoadedOne();
	        setTimeout(worker, loadDelayInterval);
	    };
	        image.src = src;
		}
	};

	// load images 6 at a time
	var concurrent = 13;
	for(var i = 0; i < concurrent; i++) {
		setTimeout(worker, 20 - i);
		};
	}

// define a function that will get called every time one image is successfully loaded
var numLoadedImages = 0;
function onLoadedOne() {
	numLoadedImages++;
	
	// $("#num-loaded").text(numLoadedImages); 

	// display progress bar
	var percentComplete = (numLoadedImages)/13 * 100;
	var percentCompleteRounded = Math.round(percentComplete);
	$('#preload .progress-bar').attr("aria-valuenow", percentComplete.toString());
	$('#preload .progress-bar').css("width", percentComplete.toString()+"%");
}

// define a function that will get called once all images have been successfully loaded
function onLoadedAll() {
  showSlide("consent");
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
addCharacter("nicholas_gannon_baby", "Nicholas Gannon, 5-Month-Old", "Nicholas is a five-month-old baby.");
addCharacter("samantha_hill_girl", "Samantha Hill, 5-Year-Old", "Samantha is a five-year-old girl who lives with her parents and older sister Jennifer.");
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
	experiment.newData.condition = surveysSlide.order[chosen].condName;
	experiment.newData.wording = surveysSlide.order[chosen].wording;
	experiment.next();
	window.scrollTo(0, 0);
});

$('.slide#demographics button').click(function() { 
	// record demographic info...
	// text inputs
	experiment.newData.age = $('input#age', '#demographicsForm').val();
	experiment.newData.job = $('input#job', '#demographicsForm').val(); 
	experiment.newData.country = $('input#country', '#demographicsForm').val();
	experiment.newData.children = $('input#children', '#demographicsForm').val();

	// text areas
	experiment.newData.comments = $('.slide#demographics textarea#comments').val();

	// multiple choice radios
	experiment.newData.gender = $('input[name=gender]:checked', '#demographicsForm').val();
	experiment.newData.education = $('input[name=education]:checked', '#demographicsForm').val();
	experiment.newData.englishNative = $('input[name=englishNative]:checked', '#demographicsForm').val();
	experiment.newData.maritalStatus = $('input[name=maritalStatus]:checked', '#demographicsForm').val();
	experiment.newData.vegetarian = $('input[name=vegetarian]:checked', '#demographicsForm').val();
	experiment.newData.studyMoralPhil = $('input[name=studyMoralPhil]:checked', '#demographicsForm').val();
	experiment.newData.politicalIdeology = $('input[name=politicalIdeology]:checked', '#demographicsForm').val();
	experiment.newData.beliefGod = $('input[name=beliefGod]:checked', '#demographicsForm').val();
	experiment.newData.beliefTradition = $('input[name=beliefTradition]:checked', '#demographicsForm').val();
	experiment.newData.beliefAfterlife = $('input[name=beliefAfterlife]:checked', '#demographicsForm').val();
	experiment.newData.beliefLeader = $('input[name=beliefLeader]:checked', '#demographicsForm').val();
	experiment.newData.beliefRules = $('input[name=beliefRules]:checked', '#demographicsForm').val();

	// multiple answer checkboxes
	$('input[name=ethnicity]:checked', '#demographicsForm').each(function() {
		experiment.newData.ethnicity.push($(this).val());
	});
	$('input[name=religionChild]:checked', '#demographicsForm').each(function() {
		experiment.newData.religionChild.push($(this).val());
	});
	$('input[name=religionNow]:checked', '#demographicsForm').each(function() {
		experiment.newData.religionNow.push($(this).val());
	});

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
	}, 1500);
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

			// fill in text on slide
			var charNum = i.toString();
			$("h2#character"+charNum).text(charactersSlide.order[i].charTitle.split(",")[0]);
			$("img#character"+charNum).attr("src", charactersSlide.order[i].imageSource);
			$("p#character"+charNum).text(charactersSlide.order[i].charDescrip);

			// store order in experiment data object
			experiment.newData.charIntroOrder.push(this.order[i].charName);

		};
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

			// fill in text on slide
			var condNum = i.toString();
			$("#surveys h2#"+condNum).text(this.order[i].condName);
			$("#surveys p#"+condNum).text("This survey asks you to judge which character is more capable of "+this.order[i].wording+".");
			$("#surveys button#"+condNum).text("Start "+this.order[i].condName+" Survey");

			// store order in experiment data object
			experiment.newData.condIntroOrder.push(this.order[i].condName);
		}
	}
}

/* set up how to display stage slide (experiment trials) */

var experiment = {
	// array for making each new trial
	trials: pairs,

	// where to store all the data
	newData: {
		// fingerprinting information
		fingerprintData: {},

		// condition and session information
		charIntroOrder: [],
		condIntroOrder: [],
		condition: "",
		wording: "",

		// demographic information about participant
		age: "",
		gender: "",
		job: "",
		education: "",
		ethnicity: [],
		religionChild: [],
		religionNow: [],
		country: "",
		englishNative: "",
		maritalStatus: "",
		children: "",
		vegetarian: "",
		studyMoralPhil: "",
		politicalIdeology: "",
		beliefGod: "",
		beliefTradition: "",
		beliefAfterlife: "",
		beliefLeader: "",
		beliefRules: "",
		comments: "",

		// trial by trial data
		trialData: [],

		// summary data for use in results slide
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
		},
		charMeans: {
			charlie_dog: NaN,
			delores_gleitman_deceased: NaN,
			fetus: NaN,
			gerald_schiff_pvs: NaN,
			god: NaN,
			green_frog: NaN,
			kismet_robot: NaN,
			nicholas_gannon_baby: NaN,
			samantha_hill_girl: NaN,
			sharon_harvey_woman: NaN,
			toby_chimp: NaN,
			todd_billingsley_man: NaN,
			you: NaN
		}
	},

	// what to do when the participant has seen all trials
	end: function() {
		showSlide("demographics");
	},

	// what happens when participant sees a new trial
	next: function() {
		if (this.trials.length === 0) {
			experiment.end();
		} else {
			// create place to store data for this trial
			var data = {
				trialNum: 79 - this.trials.length,
				leftCharacter: {},
				rightCharacter: {},
				response: "",
				rt: NaN
			};

			// assign left and right characters
			var pair = randomElementNR(this.trials);
			var sideBucket = [0,1];
			data.leftCharacter = pair[randomElementNR(sideBucket)];
			data.rightCharacter = pair[sideBucket]

			// display progress bar
			var percentComplete = (data.trialNum-1)/78 * 100;
			var percentCompleteRounded = Math.round(percentComplete);
			$('#trial-num').text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

			// set text and images for this trial
			$(".slide#stage #question").text("Which character do you think is more capable of "+this.newData.wording+"?");
			$("#stage #image-left").attr("src", data.leftCharacter.imageSource);
			$("#stage #image-right").attr("src", data.rightCharacter.imageSource);
			$("#stage #text-left").text(data.leftCharacter.charTitle);
			$("#stage #text-right").text(data.rightCharacter.charTitle);
			
			// show trial
			showSlide("stage");

			// record response and rt
			var startTime = (new Date()).getTime();

			var clickHandler = function(event) {
				var endTime = (new Date()).getTime();
				data.rt = endTime - startTime;
				experiment.newData.trialData.push(data);
			};

			$(".slide#stage button").click(function() { 
				// record response
				data.response = $(this).attr('id');

				// recode response as number
				switch (data.response) { 
					case "much more left":
						characterMore = data.leftCharacter.charName;
						characterLess = data.rightCharacter.charName;
						experiment.newData.charScores[characterMore].push(2);
						experiment.newData.charScores[characterLess].push(-2);
						break;
					case "slightly more left":
						characterMore = data.leftCharacter.charName;
						characterLess = data.rightCharacter.charName;
						experiment.newData.charScores[characterMore].push(1);
						experiment.newData.charScores[characterLess].push(-1);
						break;
					case "both equally":
						experiment.newData.charScores[data.leftCharacter.charName].push(0);
						experiment.newData.charScores[data.rightCharacter.charName].push(0);
						break;
					case "slightly more right":
						characterMore = data.rightCharacter.charName;
						characterLess = data.leftCharacter.charName;
						experiment.newData.charScores[characterMore].push(1);
						experiment.newData.charScores[characterLess].push(-1);
						break;
					case "much more right":
						characterMore = data.rightCharacter.charName;
						characterLess = data.leftCharacter.charName;
						experiment.newData.charScores[characterMore].push(2);
						experiment.newData.charScores[characterLess].push(-2);
						break;
					default: 
						console.log("whoops");
				}

				// store only character names instead of character objects
				data.leftCharacter = data.leftCharacter.charName;
				data.rightCharacter = data.rightCharacter.charName;

				// end trial
				clickHandler();
				$(".slide#stage button").unbind().blur();
				window.scrollTo(0, 0);
				experiment.next();
			})
		}
	}
}

/* set up how to display results */

var resultsSlide = {
	list: characters,
	// charScores: experiment.newData.charScores,
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
			var total = 0;
			array = experiment.newData.charScores[i];
			for (j = 0; j < array.length; j++) {
				total += array[j];
			}
			var mean = total/array.length;
			this.charMeans[i] = mean;
		};
		experiment.newData.charMeans = this.charMeans;
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
			$("p#rankingIntro").text("Here's how you ranked these characters, from most to least capable of "+experiment.newData.wording+":");
			$("p#rank"+charNum).text(characters[charName].charTitle);
			$("img#rank"+charNum).attr("src", characters[charName].imageSource);
		}
	}
}

/* preload images */
// declare the set of images we'd like to load
var images = [];
for (i = 0; i < Object.keys(characters).length; i++) {
	images[i] = characters[Object.keys(characters)[i]].imageSource;
}

preload(images,
        onLoadedOne,
        onLoadedAll); // shows consent slide