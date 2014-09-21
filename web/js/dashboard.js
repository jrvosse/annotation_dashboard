YUI().use('event', 'json', 'io', 'querystring', function(Y) {
    Y.on('domready', function() {
	Y.all(   '.judgebutton.agree.unchecked').on('click', submitJudgement, null, 'agree', 'disagree', 'add');
	Y.all('.judgebutton.disagree.unchecked').on('click', submitJudgement, null, 'disagree', 'agree', 'add');
	Y.all('.pagination a').on('click', pagination);
    });
    MOTIVATION = {
	tagging:    'http://www.w3.org/ns/oa#tagging',
	commenting: 'http://www.w3.org/ns/oa#commenting',
	moderating: 'http://www.w3.org/ns/oa#moderating',
    };
    function pagination(ev) {
	    var limit  = ev.currentTarget.getAttribute('limit');
	    var offset = ev.currentTarget.getAttribute('offset');
	    var task   = ev.currentTarget.getAttribute('task');
	    var parameters = Y.QueryString.stringify({ task:task,limit:limit, offset:offset });
	    location.assign(location.pathname + '?' + parameters);
    }
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
		var buttons = button.get('parentNode').get('parentNode').all('.judgebutton');
		var labels = button.get('parentNode').get('parentNode').all('label.btn');
		labels.removeClass('active');
		buttons.setAttribute('judgement', r.annotation['@id']);
		buttons.removeClass('checked');
		buttons.addClass('unchecked');
		buttons.on('click', submitJudgement, null, toggleto, type, 'add');
		button.detach('click');
		button.removeClass('unchecked');
		button.addClass('checked');
		button.get('parentNode').addClass('active');
	    } }
	})
    }
});
