// Function to shuffle the stimuli and assign each one a source from the same topic
function prepareRandomizedStimuli(stimuli, sources) {
  // Group sources by topic
  const sourcesByTopic = {};
  sources.forEach(source => {
    if (!sourcesByTopic[source.topic]) {
      sourcesByTopic[source.topic] = [];
    }
    sourcesByTopic[source.topic].push(source.source);
  });
  
  // Randomize source order within each topic
  for (let topic in sourcesByTopic) {
    sourcesByTopic[topic] = jsPsych.randomization.shuffle(sourcesByTopic[topic]);
  }
  
  // Index of the next source to use for each topic
  const sourceIndices = {};
  for (let topic in sourcesByTopic) {
    sourceIndices[topic] = 0;
  }
  
  // Randomize the order of stimuli
  const shuffledStimuli = jsPsych.randomization.shuffle(stimuli);
  
  // Match each stimulus with a source from the matching topic
  const preparedStimuli = shuffledStimuli.map((stimulus, index) => {
    const topic = stimulus.topic;
    
    // Reuse sources if a topic runs out
    if (!sourcesByTopic[topic] || sourceIndices[topic] >= sourcesByTopic[topic].length) {
      console.warn(`Warning: Not enough sources for topic "${topic}". Reusing sources.`);
      sourceIndices[topic] = 0;
    }
    
    // Get the next unused source for this topic
    const assignedSource = sourcesByTopic[topic][sourceIndices[topic]];
    sourceIndices[topic]++;
    
    // Create the combined object with stimulus info and assigned source
    return {
      post_id: stimulus.post_id,
      stance: stimulus.stance,
      text: stimulus.text,
      truth: stimulus.truth,
      topic: stimulus.topic,
      source: assignedSource,
      author: assignedSource,
      avatar: assignedSource.charAt(0),
      content: stimulus.text,
      true_value: stimulus.truth === 'true',
      index: index
    };
  });
  
  return preparedStimuli;
}
