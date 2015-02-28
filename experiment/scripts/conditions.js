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

// set up button behaviors for surveys slide

$('.slide#surveys button').click(function() { // select condition
	var chosen = $(this).attr('id');
	experiment.newData.condition = surveysSlide.order[chosen].condName;
	experiment.newData.wording = surveysSlide.order[chosen].wording;
	experiment.next();
	window.scrollTo(0, 0);
});

// set up how to display surveys slide

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