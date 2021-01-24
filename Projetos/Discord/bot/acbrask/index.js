//  Discord bot  
//  Name: ACBr Ask
//  Explanation: This robot can ask questions when started with a Prefix, or if it is mentioned on message.
//               He also can query all arguents on message and examine them against the existing questions structure
//  Author: Daniel Simões de Almeida - 17/01/2021
//  Inspiration: https://www.digitalocean.com/community/tutorials/how-to-build-a-discord-bot-with-node-js-pt
// 

//questions.json
/*
[
 {
  "keywords": "baixar|encontrar|download +dll|dlls|bibliotecas +tef", -> Use Spaces for keywords separator. Use "|" for OR condition. Use "+" to oblige the presence
  "mention": false,                                                   -> Only check this questions if the Bot is mentioned on message
  "sequence": false,                                                  -> If True, Look for questions words in the specific order informed
  "score": 70,                                                        -> Percentege of Minimun of Score Hits to considere this question
  "msg": "Você pode baixar o SDK atualizado do TEF https://projetoacbr.com.br/tef"    -> The message to send as Answer
 },
 {
  "keywords": "baixar|encontrar|download +acbrmonitor|acbrmonitorplus",
  "mention": false, 
  "sequence": false,
  "score": 100,
  "msg": "Você pode baixar o SDK atualizado do TEF [nesse link](https://projetoacbr.com.br/acbrmonitor/)"
 } 
]
*/


const Discord = require('discord.js');
const botConf = require('./botConfig.json');
const questions = require('./questions.json');

const client = new Discord.Client();

function doLog(...values) {
  if (botConf.log) {
    console.log(values);
  }
}

client.on('ready', () => {
  client.user.setStatus('online');
  console.log(`Logged in as ${client.user.tag}!`);
});

client.on('message', function(message) {
  if (message.author.bot) {
    doLog('no bots');
    return;
  }

  const status = client.user.presence.status;  
  if(status != "online"){
    client.user.setStatus('online');
  }
  
  doLog('\n--------------------',);
  doLog('message.content:',message.content);
  
  var commandBody = "";
  var isBotMentioned =  false;
  
  // Has the bot prefix ?
  if (message.content.startsWith(botConf.prefix)) {
    doLog('got prefix', botConf.prefix)
    // Remove Bot Prefix from Message
    commandBody = message.content.slice(botConf.prefix.length);
    isBotMentioned = true;
  }

  // The Bot is Mentioned on message ? 
  if ((!message.mentions.everyone) && (message.mentions.has(client.user, false, true, true))) {
    doLog(client.user.username+' is mentioned')
    commandBody = message.content;
    // Remove Bot name from Message
    commandBody = commandBody.replace('<@!'+client.user.id+'>', '');
    isBotMentioned = true;
  }

  if (!isBotMentioned) {
    // No Mention or Prefix for Bot on Message. Check if the bot can answer questions with no mention for him...
    var canTryToAnswer = false;
    for(var q of questions) {
      canTryToAnswer = !q.mention;
      if (canTryToAnswer) break;
    }      
      
    if (!canTryToAnswer) {
      doLog('No mention, exiting...');
      return
    } else {
      doLog('examining arguments...');
      commandBody = message.content;
    }
  }
  
  // Remove punctuation Chars at the end
  while ('?!.'.indexOf(commandBody.slice(-1)) > -1) {
    commandBody = commandBody.slice(0, commandBody.length-1);
  }
	
  commandBody = commandBody.trim().toLowerCase();
  doLog('commandBody: '+commandBody);

  // Splitting arguments, and removing empty ones...
  const argsTemp = commandBody.split(/ +/);
  var args = [];
  for(var a of argsTemp) {
    a = a.trim();
    if (a.length > 0){
      args.push(a);
    }
  }
      
  if (args.length == 0) {
    doLog('No arguments, exiting...');
    return
  }
  
  var command = '';
  if (isBotMentioned) {
    command = args[0];
  }
  
  if (botConf.daysGreeting > 0) {
    const today = new Date();
    const daysFromLastMessage = (today - message.author.lastMessage.createdAt) / (1000 * 3600 * 24);
    doLog('daysFromLastMessage', daysFromLastMessage);
    if (daysFromLastMessage > botConf.daysGreeting) {
      message.reply(' seja bem vindo de Volta... Fazem '+daysFromLastMessage+' dias que não vemos você aqui');
    }
  }
  
  doLog('command',command);
  doLog('args:',args);
  
  if (command === 'ping') {
    doLog('ping');
    const timeTaken = Date.now() - message.createdTimestamp;
    message.reply(' \n :ping_pong: \n Pong! A lantência foi '+timeTaken+' ms.');
    return
  }

  if (command === 'avatar') {
    doLog('avatar');
    message.reply(message.author.displayAvatarURL());
    return
  }
  
  if (command.startsWith('ola')) {
    message.reply(' ola');
    return
  }

  if (command === 'data') {
    const data = new Date();
    const dataFormatada = ((data.getDate() )) + "/" + ((data.getMonth() + 1)) + "/" + data.getFullYear(); 
    message.reply('hoje é dia '+dataFormatada);
    return
  }
  
  if (command.startsWith('hora')) {
    const data = new Date();
    const horaFormatada = ((data.getHours() )) + ":" + ((data.getMinutes() + 1)) + ":" + data.getSeconds(); 
    message.reply('agora são '+horaFormatada);
    return
  }
  
  if (command === 'perguntas'){
    var listaPerguntas = 'segue a lista de parâmetros para as perguntas que conheço \n ';
    for(var item of questions) {
     listaPerguntas = listaPerguntas + item.keywords + ' \n ';
    }
    
    message.reply(`${listaPerguntas}`);
    return
  }
  
  function findWordinArguments(word, args, startIn){
    var isWordOblige = false;
    var af = -1;
    
    doLog('  findWordinArguments', word, args, startIn);

    if (word.startsWith('+')) {
      doLog('    isWordOblige');
      word = word.slice(1);
      isWordOblige = true;
    }

    var wordOr = word.split("|");
    doLog('    wordOr', wordOr);
    
    for(var w of wordOr) {
      af = args.indexOf(w);
      doLog('    found', w, af);
      if(af>=0)break;  // found
    }

    if ((af<0) && (isWordOblige)){
      doLog('    Word oblige not found', wordOr);
      af = -2;
    }

    doLog('  return', af);
    
    return af;
  }
                               
  // Ok Let's examine the arguments and check them against the existing questions
  var i = 0;
  var qi = -1;
  var questionScore = [];  // questionScore is an array of percentegeScored
  
  for(var q of questions) {
    qi = qi+1;
    questionScore.push(-1);
    
    // If the Bot is not mentioned and these question demands a Mention... skip it.. 
    if ((!isBotMentioned) && (q.mention)){
      doLog('  question demands mention, and Bot is not mentioned');
      continue;
    }
    
    var words = q.keywords.split(/ +/);
    var hits = 0;
    var af = -1;
    var alf = -1;
    
    for(var w of words) {
      af = findWordinArguments(w, args, alf);
      
      if (af == -2){                  // oblige word not found 
        hits = -1;
        break;
      }
      
      if (af > -1){                   // word is found..
        if (q.sequence) {                // Words must be found on correct order ?
          if (af < alf) {                // ...but was found in an order below last one... Invalidate the hit...
            doLog('  question demands order, and word is in wrong order');
            af = -1;
          }
        }
      }
    
      if (af > -1) {                  // a word is found..
        hits = hits+1;
        alf = af;                     // save the last word found position
        doLog('  hits', hits);
      }
    }
    
    if (hits < 0) {                   // oblige word not found 
      questionScore[qi] = -1;
      doLog('question Discard', qi);
    } else { 
      questionScore[qi] = (hits / words.length) * 100;        // Compute the Question Score
      doLog('question Score', qi, questionScore[qi]);
      
      if (questionScore[qi] < q.score) {                      // We reach the Minimum Score ?
        doLog('question Score not reached, discard', q.score);
        questionScore[qi] = -1;
      }
    }
  }
  
  // Whats is our Best question Score ?
  var bestScore = -1;
  var bestHits = -1;
  var ni = -1;
  qi = -1;
  for (var i = 0; i < questionScore.length; i++) {
    ni = -1;
    
    if (questionScore[i] <= 0){
      continue
    };
          
    if (questionScore[i] = bestScore) {        // A Draw
      if (botConf.draw = 0) {                  // Draw, use the Question with more Words..
        const w1 = questions[qi].keywords.split(/ +/);
        const w2 = questions[i].keywords.split(/ +/);
        if (w2 >= w1) {
          ni = i;
        }        
      } else if (botConf.draw = 1) {           // Draw, use the Higher Question in List
        ni = i;
      } 
    } else if (questionScore[i] > bestScore) {
      ni = i;
    }
    
    if (ni > -1) {
      bestScore = questionScore[ni];
      qi = ni;
      doLog('  new bestScore', bestScore, qi);
    }
  }
  doLog('the bestScore is', bestScore, qi);

  if (qi > -1) {
    var msgs = questions[qi].msg;
    var i = msgs.length;
    if (i > 1) {                        // If we have several answers, we will raffle one...
      doLog('raffle', i);
      i = Math.floor(Math.random() * (i - 1)) + 1;
    }
        
    doLog('show', qi, i-1, questions[qi].msg[i-1]);
    message.reply(questions[qi].msg[i-1]);
  } else {
    if (isBotMentioned) {
      doLog('unknown');
      message.reply('desculpe, mas não conheço o comando: **'+commandBody+'**');
    }
  }
});

client.login(botConf.token);

