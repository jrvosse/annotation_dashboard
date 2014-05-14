YUI().use('event', 'json', 'io', function(Y) {
    Y.on('domready', function() {
	Y.all('.agreeButton.unchecked').on('click', submitJudgement, 'agree', 'add');
    });
    MOTIVATION = {
	tagging:    'http://www.w3.org/ns/oa#tagging',
	commenting: 'http://www.w3.org/ns/oa#commenting',
	moderating: 'http://www.w3.org/ns/oa#moderating',
    };
    function submitJudgement(ev, type, mode) {
	
	var annotationId = ev.currentTarget.getAttribute('title');
	var fieldId = ev.currentTarget.one('img').getAttribute('title');
	var body = {'@value': type };
	var bodyString = Y.JSON.stringify(body);
	var targetObject = [{'@id':annotationId}]
	var targetString = Y.JSON.stringify(targetObject);
	Y.log(type);
	Y.log(mode);
	Y.io('../../api/annotation/add', {
	    method: 'POST',
	    data: {
		field: fieldId,
		hasTarget:targetString,
		hasBody:bodyString,
		label:type,
		typing_time: -1,
		motivatedBy: MOTIVATION.moderating
	    },
	    on:{ success: function(e,o) {
		Y.log(e);
		Y.log(o);
	    }
	       }
	})
    }
});

