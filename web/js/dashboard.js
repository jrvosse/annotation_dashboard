YUI().use('event', 'json', 'io', function(Y) {
    Y.on('domready', function() {
	Y.all(   '.agreeButton.unchecked').on('click', submitJudgement, null, 'agree', 'disagree', 'add');
	Y.all('.disagreeButton.unchecked').on('click', submitJudgement, null, 'disagree', 'agree', 'add');
    });
    MOTIVATION = {
	tagging:    'http://www.w3.org/ns/oa#tagging',
	commenting: 'http://www.w3.org/ns/oa#commenting',
	moderating: 'http://www.w3.org/ns/oa#moderating',
    };
    function submitJudgement(ev, type, toggleto, mode) {
	var button = ev.currentTarget;
	button.detach('click');

	var annotationId = button.getAttribute('annotation');
	var fieldId      = button.getAttribute('field');
	var previous     = button.getAttribute('judgement');
	var body = {'@value': type };
	var bodyString = Y.JSON.stringify(body);
	var targetObject = [{'@id':annotationId}]
	var targetString = Y.JSON.stringify(targetObject);
	button.removeAttribute('judgement');
	if (previous && previous != 'null') Y.io('../../api/annotation/remove', {
	    method: 'DELETE',
	    data: { annotation: previous, comment: 'overruled by new judgement ' + type },
	    on:{ success: function(e,o) {
	    } }
	});
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
		var r = Y.JSON.parse(o.responseText);
		var buttons = button.get('parentNode').all('.judgeButton');
		var peer = null;
		if (buttons.item(0) == button) peer = buttons.item(1);
		if (buttons.item(1) == button) peer = buttons.item(0);
		buttons.setAttribute('judgement', r.annotation['@id']);
		button.removeClass('unchecked');
		button.addClass('checked');
		peer.addClass('unchecked');
		peer.removeClass('checked');
		peer.on('click', submitJudgement, null, toggleto, type, 'add');
	    } }
	})
    }
});

