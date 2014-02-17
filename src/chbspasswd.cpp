// chbspasswd v0.2 - chbspasswd.cpp
//
// A password generator inspired by XKCD 936: Password Strength and xkpasswd.net
//
// Copyright (C) 2013-2014, Charles H. Leggett
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "chbspasswd.hpp"

#include <sstream>
#include <algorithm>
#include <stdlib.h>
#include <sys/time.h>

// BEGIN: CHBSPassword PUBLIC

CHBSPassword::CHBSPassword() {

  // Seed the pseudo-random number generator.
  struct timeval tv;
  gettimeofday(&tv, NULL);
  srandom (tv.tv_sec * tv.tv_usec);

  // Define valid character type values
  validPadDigits = "0123456789";
  validPadSpecialCharacters = "!@#$%^&*?";
  validSeparators = "~.-_=+:";

  // Valid word lengths initalized to 1 here and will be updated to the length
  // of the longest and shortest word in the dictionary via buildDictionary();
  validWordMinimumLength = 1;
  validWordMaximumLength = 1;

  // separatorSaved should always be an empty string in this constructor. A
  // seperator string will be saved to it in getSeparator() if using the same
  // separator is specified.
  separatorSaved = "";

  // padDefaultsOverridden should always be false in this constructor. It will
  // be set to true if _any_ pad option is specified on the commandline b/c if
  // so, all pad options should be disabled and then set to the options
  // specified
  padDefaultsOverridden = false;

  // Set defaults for password to be similar to: AlsoCaptainHasParallel
  wordCount = 4;
  wordMinimumLength = 3;
  wordMaximumLength = 8;
  wordCase = "INITIAL";

  eleetEnabled = false;

  separatorEnabled = false;
  separatorType = "SAME";
  separatorCount = 1;

  beforeEnabled = false;
  beforeType = "MIXED";
  beforeCount = 1;

  insideEnabled = false;
  insideType = "MIXED";
  insideCount = 1;

  afterEnabled = false;
  afterType = "MIXED";
  afterCount = 1;

}

void CHBSPassword::buildDictionary () {

  // This string has been manually copied from dictionary.txt It is NOT loaded
  // dynamically. I tried to use a #include, but I couldn't get it to work.  If
  // there is a better way to do this, please let me know: charles@solarturtle.net

  std::string dictionary = "a,ability,able,aboard,about,above,accept,accident,according,account,accurate,acres,across,act,action,active,activity,actual,actually,add,addition,additional,adjective,adult,adventure,advice,affect,afraid,Africa,after,afternoon,again,against,age,ago,agree,ahead,aid,air,airplane,Alaska,Alice,alike,alive,all,allow,almost,alone,along,aloud,alphabet,already,also,although,am,America,among,amount,an,ancient,and,Andy,angle,angry,animal,Ann,announced,another,answer,ants,any,anybody,anyone,anything,anyway,anywhere,apart,apartment,appearance,apple,applied,appropriate,April,are,area,arm,army,around,arrange,arrangement,arrive,arrow,art,article,as,Asia,aside,ask,asleep,at,ate,Atlantic,atmosphere,atom,atomic,attached,attack,attempt,attention,audience,August,Aunt,Australia,author,automobile,Autumn,available,average,avoid,aware,away,baby,back,bad,badly,bag,balance,ball,balloon,band,bank,bar,bare,bark,barn,base,baseball,basic,basis,basket,bat,battle,Bay,be,bean,bear,beat,beautiful,beauty,became,because,become,becoming,bee,been,before,began,beginning,begun,behavior,behind,being,believed,bell,belong,below,belt,Ben,bend,beneath,bent,beside,best,bet,Betsy,better,between,beyond,bicycle,bigger,biggest,Bill,Billy,birds,birth,birthday,bit,bite,black,blank,blanket,blew,blind,block,blood,blow,blue,board,boat,Bob,body,bone,book,border,born,both,bottle,bottom,bound,bow,bowl,box,boy,brain,branch,brass,brave,bread,break,breakfast,breath,breathe,breathing,breeze,brick,bridge,brief,bright,bring,British,broad,broke,broken,brother,brought,brown,brush,buffalo,build,building,built,buried,burn,burst,bus,bush,business,busy,but,butter,buy,by,cabin,cage,cake,California,call,calm,came,camera,camp,can,Canada,canal,cannot,cap,capital,captain,captured,car,carbon,card,care,careful,carefully,Carlos,carried,carry,case,Casey,cast,castle,cat,catch,cattle,caught,cause,cave,cell,cent,center,central,century,certain,certainly,chain,chair,chamber,chance,change,changing,chapter,character,characteristic,charge,Charles,chart,check,cheese,chemical,chest,Chicago,chicken,chief,child,children,China,Chinese,choice,choose,chose,chosen,Christmas,church,circle,circus,citizen,city,class,classroom,claws,clay,clean,clear,clearly,climate,climb,clock,close,closely,closer,cloth,clothes,clothing,cloud,club,coach,coal,coast,coat,coffee,cold,collect,college,colony,color,Columbus,column,combination,combine,come,comfortable,coming,command,common,community,company,compare,compass,complete,completely,complex,composed,composition,compound,concerned,condition,congress,connected,consider,consist,consonant,constantly,construction,contain,continent,continued,contrast,control,conversation,cook,cookies,cool,copper,copy,corn,corner,correct,correctly,cost,cotton,could,count,country,couple,courage,course,court,cover,cow,cowboy,crack,cream,create,creature,crew,crop,cross,crowd,cry,cup,curious,current,curve,customs,cut,cutting,Dad,daily,damage,Dan,dance,danger,dangerous,Daniel,Danny,dark,darkness,date,daughter,David,dawn,day,dead,deal,dear,death,decide,declared,deep,deeply,deer,definition,degree,depend,depth,describe,desert,design,desk,detail,determine,develop,development,diagram,diameter,Dick,did,die,differ,difference,different,difficult,difficulty,dig,dinner,direct,direction,directly,dirt,dirty,disappear,discover,discovery,discuss,discussion,disease,dish,distance,distant,divide,division,do,doctor,does,dog,doing,doll,dollar,Don,done,donkey,door,dot,double,doubt,down,dozen,draw,drawn,dream,dress,drew,dried,drink,drive,driven,driver,driving,drop,dropped,drove,dry,duck,due,dug,dull,during,dust,Dutch,duty,each,eager,ear,earlier,early,earn,earth,easier,easily,east,easy,eat,eaten,Eddy,edge,education,Edward,effect,effort,egg,Egypt,eight,either,electric,electricity,element,elephant,eleven,Ellen,else,empty,end,enemy,energy,engine,engineer,England,enjoy,enough,enter,entire,entirely,environment,equal,equally,equator,equipment,escape,especially,essential,establish,Europe,European,even,evening,event,eventually,ever,every,everybody,everyone,everything,everywhere,evidence,exact,exactly,examine,example,excellent,except,exchange,excited,excitement,exciting,exclaimed,exercise,exist,expect,experience,experiment,explain,explanation,explore,express,expression,extra,eye,face,facing,fact,factor,factory,failed,fair,fairly,fall,fallen,familiar,family,famous,far,farm,farmer,farther,fast,fastened,faster,fat,father,favorite,fear,feathers,feature,fed,feed,feel,feet,fell,fellow,felt,fence,few,fewer,field,fierce,fifteen,fifth,fifty,fight,fighting,figure,fill,film,final,finally,find,fine,finest,finger,finish,fire,fireplace,firm,first,fish,five,fix,flag,flame,flat,flew,flies,flight,floating,floor,Florida,flow,flower,fly,fog,folks,follow,food,foot,football,for,force,foreign,forest,forget,forgot,forgotten,form,former,fort,forth,forty,forward,fought,found,four,fourth,fox,frame,France,Frank,Fred,free,freedom,French,frequently,fresh,friend,friendly,frighten,frog,from,front,frozen,fruit,fuel,full,fully,fun,function,funny,fur,furniture,further,future,gain,game,garage,garden,gas,gasoline,gate,gather,gave,general,generally,gentle,gently,George,German,Germany,get,getting,giant,gift,girl,give,given,giving,glad,glass,globe,go,goes,gold,golden,gone,good,goose,got,government,grabbed,grade,gradually,grain,grandfather,grandmother,graph,grass,gravity,gray,great,greater,greatest,greatly,Greece,Greek,green,grew,ground,group,grow,grown,growth,guard,guess,guide,gulf,gun,habit,had,hair,half,halfway,hall,hand,handle,handsome,hang,happen,happened,happily,happy,harbor,hard,harder,hardly,Harry,has,hat,have,having,hay,he,headed,heading,health,heard,hearing,heart,heat,heavy,height,held,hello,help,helpful,Henry,her,herd,here,herself,hidden,hide,high,higher,highest,highway,hill,him,himself,his,history,hit,hold,hole,hollow,home,honor,hope,horn,horse,hospital,hot,hour,house,how,however,huge,human,hundred,hung,hungry,hunt,hunter,hurried,hurry,hurt,husband,I,ice,idea,identity,if,ill,Illinois,image,imagine,immediately,importance,important,impossible,improve,in,inch,include,including,income,increase,indeed,independent,India,Indian,indicate,individual,industrial,industry,influence,information,inside,instance,instant,instead,instrument,interest,interior,into,introduced,invented,involved,iron,is,island,it,Italian,Italy,its,itself,jack,James,Jane,January,Japan,Japanese,jar,Jeff,jet,Jim,Jimmy,job,Joe,John,Johnny,Johnson,join,joined,Jones,journey,joy,judge,July,jump,June,jungle,just,keep,kept,key,kids,kill,kind,King,kitchen,knew,knife,know,knowledge,known,label,labor,lack,lady,laid,lake,lamp,land,language,large,larger,largest,last,late,later,Latin,laugh,law,lay,layers,lead,leader,leaf,learn,least,leather,leave,leaving,led,Lee,left,leg,length,lesson,let,letter,level,library,lie,life,lift,light,like,likely,limited,Lincoln,line,lion,lips,liquid,list,listen,little,live,living,load,local,locate,location,log,London,lonely,long,longer,look,loose,lose,loss,lost,lot,loud,Louis,love,lovely,low,lower,luck,lucky,lunch,lungs,lying,machine,machinery,mad,made,magic,magnet,mail,main,mainly,major,make,making,Mama,man,managed,manner,manufacturing,many,map,March,Maria,Mark,market,married,Mars,Martin,Mary,mass,massage,master,material,mathematics,matter,May,maybe,me,meal,mean,means,meant,measure,meat,medicine,meet,melted,member,memory,men,mental,merely,met,metal,method,Mexico,mice,middle,might,mighty,Mike,mile,military,milk,mill,mind,mine,minerals,minute,mirror,Miss,missing,mission,Mississippi,mistake,mix,mixture,model,modern,molecular,moment,money,monkey,month,mood,moon,more,morning,most,mostly,mother,motion,motor,Mount,mountain,mouse,mouth,move,movement,movie,moving,mud,muscle,music,musical,must,my,myself,mysterious,nails,name,nation,national,native,natural,naturally,nature,near,nearby,nearer,nearest,nearly,necessary,neck,needed,needle,needs,negative,Negro,neighbor,neighborhood,nervous,nest,never,new,news,newspaper,next,nice,night,nine,no,nobody,nodded,noise,none,noon,nor,north,Norway,nose,not,note,noted,nothing,notice,noun,now,number,numeral,nuts,object,observe,obtain,occasionally,occur,ocean,October,of,off,offer,office,officer,official,Ohio,oil,old,older,oldest,on,once,one,only,onto,open,operation,opinion,opportunity,opposite,or,orange,orbit,order,ordinary,organization,organized,origin,original,other,ought,our,ourselves,out,outer,outline,outside,over,own,owner,oxygen,Pacific,pack,package,page,paid,pain,paint,pair,palace,pale,pan,Papa,paper,paragraph,parallel,parent,Paris,park,part,particles,particular,particularly,partly,parts,party,pass,passage,past,path,pattern,Paul,pay,peace,pen,pencil,Pennsylvania,people,per,percent,perfect,perfectly,perhaps,period,person,personal,pet,Peter,Philadelphia,phrase,physical,piano,pick,picture,pictured,pie,piece,pig,pile,pilot,pine,pink,pipe,pitch,place,plain,plan,plane,planet,planned,planning,plant,plastic,plate,plates,play,pleasant,please,pleasure,plenty,plural,plus,pocket,poem,poet,poetry,point,Pole,police,policeman,political,pond,pony,pool,poor,popular,population,porch,port,position,positive,possible,possibly,post,pot,potatoes,pound,pour,powder,power,powerful,practical,practice,prepare,present,president,press,pressure,pretty,prevent,previous,price,pride,primitive,principal,principle,printed,private,prize,probably,problem,process,produce,product,production,program,progress,promised,proper,properly,property,protection,proud,prove,provide,public,pull,pupil,pure,purple,purpose,push,put,putting,quarter,queen,question,quick,quickly,quiet,quietly,quite,rabbit,race,radio,railroad,rain,raise,ran,ranch,range,rapidly,rate,rather,raw,rays,reach,read,reader,ready,real,realize,rear,reason,recall,receive,recent,recently,recognize,record,red,refer,refused,region,regular,related,relationship,religious,remain,remarkable,remember,remove,repeat,replace,replied,report,represent,require,research,respect,rest,result,return,review,rhyme,rhythm,rice,rich,Richard,ride,riding,right,ring,rise,rising,river,road,roar,Robert,rock,rocket,rocky,rod,roll,Roman,Rome,roof,room,root,rope,rose,rough,round,route,row,rubbed,rubber,rule,ruler,run,running,rush,Russia,Russian,sad,saddle,safe,safety,said,sail,sale,Sally,salmon,salt,Sam,same,sand,sang,sat,satellites,satisfied,Saturday,save,saved,saw,say,scale,scared,scene,school,science,scientific,scientist,score,screen,sea,search,season,seat,second,secret,section,see,seed,seeing,seems,seen,seldom,select,selection,sell,send,sense,sent,sentence,separate,series,serious,serve,service,sets,setting,settle,settlers,seven,several,shade,shadow,shake,shaking,shall,shallow,shape,share,sharp,she,sheep,sheet,shelf,shells,shelter,shine,shinning,ship,shirt,shoe,shoot,shop,shore,short,shorter,shot,should,shoulder,shout,show,shown,shut,sick,sides,sight,sign,signal,silence,silent,silk,silly,silver,similar,simple,simplest,simply,since,sing,single,sink,Sir,sister,sit,sitting,situation,six,size,skill,skin,sky,slabs,slave,sleep,slept,slide,slight,slightly,slip,slipped,slope,slow,slowly,small,smaller,smallest,smell,smile,Smith,smoke,smooth,snake,snow,so,soap,social,society,soft,softly,soil,solar,sold,soldier,solid,solution,solve,some,somebody,somehow,someone,something,sometime,somewhere,son,song,soon,sort,sound,source,south,southern,space,Spain,speak,special,species,specific,speech,speed,spell,spend,spent,spider,spin,spirit,spite,split,spoken,sport,spread,spring,square,stage,stairs,stand,standard,star,stared,start,state,statement,States,station,stay,steady,steam,steel,steep,stems,step,stepped,stick,stiff,still,stock,stomach,stone,stood,stop,stopped,store,storm,story,stove,straight,strange,stranger,straw,stream,street,strength,stretch,strike,string,strip,strong,stronger,struck,structure,struggle,stuck,student,studied,studying,subject,substance,success,successful,such,sudden,suddenly,sugar,suggest,suit,sum,summer,sun,Sunday,sunlight,supper,supply,support,suppose,sure,surface,surprise,surrounded,swam,sweet,swept,swim,swimming,swing,swung,syllable,symbol,system,table,tail,take,taken,tales,talk,tall,tank,tape,task,taste,taught,tax,tea,teach,teacher,team,tears,teeth,telephone,television,tell,temperature,ten,tent,term,terrible,test,Texas,than,thank,that,the,thee,them,themselves,then,theory,there,therefore,these,they,thick,thin,thing,think,third,thirty,this,Thomas,those,thou,though,thought,thousand,thread,three,threw,throat,through,throughout,throw,thrown,thumb,thus,thy,tide,tie,tight,tightly,till,Tim,time,tin,tiny,tip,tired,title,to,tobacco,today,together,told,Tom,tomorrow,tone,tongue,tonight,too,took,tool,top,topic,torn,total,touch,toward,tower,town,toy,trace,track,trade,traffic,trail,train,transportation,trap,travel,treated,tree,triangle,tribe,trick,tried,trip,troops,tropical,trouble,truck,trunk,truth,try,tube,tune,turn,TV,twelve,twenty,twice,two,type,typical,uncle,under,underline,understanding,unhappy,union,unit,United,universe,University,unknown,unless,until,unusual,up,upon,upper,upward,us,use,useful,using,usual,usually,valley,valuable,value,vapor,variety,various,vast,vegetable,verb,vertical,very,vessels,victory,view,village,Virginia,visit,visitor,voice,volume,vote,vowel,voyage,wagon,wait,walk,wall,want,war,warm,warn,was,wash,Washington,waste,watch,water,wave,way,we,weak,wealth,wear,weather,week,weigh,weight,welcome,well,went,were,west,western,wet,whale,what,whatever,wheat,wheel,when,whenever,where,wherever,whether,which,while,whispered,whistle,white,who,whole,whom,whose,why,wide,widely,wife,wild,will,William,willing,Wilson,win,wind,window,wing,winter,wire,wise,wish,with,within,without,wolf,women,won,wonder,wonderful,wood,wooden,wool,word,wore,work,worker,world,worried,worry,worse,worth,would,wrapped,write,writer,writing,written,wrong,wrote,yard,year,yellow,yes,yesterday,yet,you,young,younger,your,yourself,youth,zero,zoo";

  // Step through an input string stream of dictionary words.
  std::istringstream iss ( dictionary );

  std::string word;

  while ( std::getline ( iss, word, ',' ) ) {

    // Watch for words that are shorter than the current minimum length allowed
    // and set the minimum length if found.
    if ( word.length() < validWordMinimumLength ) {

      validWordMinimumLength = (unsigned int) word.length();

    }

    // Watch for words that are longer than the current maximum length allowed
    // and set the maximum length if found.
    if ( word.length() > validWordMaximumLength ) {

      validWordMaximumLength = (unsigned int) word.length();

    }

    // Add each word to the "words" vector declared in the class.
    words.push_back(word);

  }

}

int CHBSPassword::showDEBUG() {

  // DEBUG - Display a message with debugging information

  std::cout << std::endl;
  std::cout << "wordCount: " << wordCount << std::endl;
  std::cout << std::endl;
  std::cout << "wordCase: " << wordCase << std::endl;
  std::cout << std::endl;
  std::cout << "eleetEnabled: " << eleetEnabled << std::endl;
  std::cout << std::endl;
  std::cout << "validWordMinimumLength: " << validWordMinimumLength << std::endl;
  std::cout << "wordMinimumLength: " << wordMinimumLength << std::endl;
  std::cout << std::endl;
  std::cout << "validWordMaximumLength: " << validWordMaximumLength << std::endl;
  std::cout << "wordMaximumLength: " << wordMaximumLength << std::endl;
  std::cout << std::endl;
  std::cout << "validSeparators: " << validSeparators << std::endl;
  std::cout << "separatorEnabled: " << separatorEnabled << std::endl;
  std::cout << "separatorType: " << separatorType << std::endl;
  std::cout << "separatorCount: " << separatorCount << std::endl;
  std::cout << std::endl;
  std::cout << "validPadDigits: " << validPadDigits << std::endl;
  std::cout << "validPadSpecialCharacters: " << validPadSpecialCharacters << std::endl;
  std::cout << "padDefaultsOverridden: " << padDefaultsOverridden << std::endl;
  std::cout << std::endl;
  std::cout << "beforeEnabled: " << beforeEnabled << std::endl;
  std::cout << "beforeType: " << beforeType << std::endl;
  std::cout << "beforeCount: " << beforeCount << std::endl;
  std::cout << std::endl;
  std::cout << "insideEnabled: " << insideEnabled << std::endl;
  std::cout << "insideType: " << insideType << std::endl;
  std::cout << "insideCount: " << insideCount << std::endl;
  std::cout << std::endl;
  std::cout << "afterEnabled: " << afterEnabled << std::endl;
  std::cout << "afterType: " << afterType << std::endl;
  std::cout << "afterCount: " << afterCount << std::endl;
  std::cout << std::endl;

  return 0;

}

std::string CHBSPassword::getPassword() {

  // Build and return password based on the defaults and modifying switches.

  std::string password = "";
  int Count = wordCount;

  // Add a before string if enabled
  if ( beforeEnabled == true ) {
    password += getBefore();
    if ( wordCount >= 1 || afterEnabled == true ) {
      password += getSeparator();
    }
  }

  // If one or zero words are requested the -i option is invalid because an
  // inside string is a separator betweem words.
  if ( Count <= 1 && insideEnabled == true ) {

    std::cout << "unexpected option -- i" << std::endl;
    std::cout << "An INSIDE string is a seperator between WORDS." << std::endl;
    std::cout << "If either one (-w 1) or zero (-w 0) words are requested then -i is invalid." << std::endl;
    std::cout << std::endl;

    return "";

  }

  // While there are more than one words to add, first add a word and then add
  // either a separated inside string (if enabled) or another seperator
  while ( Count > 1 ) {

    password += getWord();

    if ( insideEnabled ) {
      password += getSeparator();
      password += getInside();
      password += getSeparator();
    }
    else {
      password += getSeparator();
    }
    Count--;
  }

  // If there is only one word is requested, or if there is only one left, just
  // add a word.
  if ( Count == 1 ) {
    password += getWord();
  }

  // Add a after string if enabled
  if ( afterEnabled ) {
    if ( wordCount >= 1 ) {
      password += getSeparator();
    }
    password += getAfter();
  }

  // Reset separatorSaved so that subsequent calls will be initialized with a
  // different separator.
  separatorSaved = "";

  return password;

}

void CHBSPassword::setWordCount ( std::string CountString ) {

  // words - Set the number of words to use.

  // Convert the string into an integer
  int Count = convertNumber ( CountString );

  // Be sure that the count is greater than 0 else
  // display a guiding error message
  if ( Count > 0 ) {

    wordCount = Count;

  }
  else {

    std::cout << "./chbspasswd: unexpected argument \"" << CountString << "\" for option -- w" << std::endl;
    std::cout << "./chbspasswd: argument must be a number with a value greater than or equal to 1" << std::endl;
    std::cout << std::endl;

    exit(0);

  }

}

void CHBSPassword::setWordCase ( std::string CASE ) {

  // "case" is a c++ reserved word, so using "CASE" and "Case" against
  // convention.

  // Standardize input for validation
  std::string Case = convertType ( CASE );

  // Validate input against defined word case types and set if it matches or
  // exit program if it does not (error message shown in validating function).
  if ( isValidWordCase ( Case ) ) {

    wordCase = Case;

  }
  else {

    exit(0);

  }

}

void CHBSPassword::setWordLength ( std::string minimumString, std::string maximumString ) {

  // Convert numeric input strings to integers
  int minimum = convertNumber ( minimumString );
  int maximum = convertNumber ( maximumString );

  // Check to be sure that the requested minimum is less than the requested
  // maximum and that the lengths are avaliable in the dictionary and provide a
  // guiding error message if not.

  if ( minimum <= maximum && minimum >= validWordMinimumLength && maximum <= validWordMaximumLength ) {

    wordMinimumLength = minimum;
    wordMaximumLength = maximum;

  }
  else {

    std::cout << "./chbspasswd: unexpected argument \"" << minimumString << "," << maximumString << "\" for option -- l" << std::endl;
    std::cout << "./chbspasswd: argument should be two numbers separated by a comma" << std::endl;
    std::cout << "./chbspasswd: the minimum length must be less than or equal to the maximum length" << std::endl;
    std::cout << "./chbspasswd: the minimum length must be greater than or equal to: " << validWordMinimumLength << std::endl;
    std::cout << "./chbspasswd: the maximum length must be less than or equal to: " << validWordMaximumLength << std::endl;
    std::cout << std::endl;

    exit(0);

  }

}

void CHBSPassword::setSeparator( std::string Type, std::string CountString ) {

  // seperator - Set the preferences and count of seperator characters between
  // words.

  // Convert the type string to uppercase for validation
  std::string type = convertType ( Type );

  // Convert the numeric string to an integer
  int Count = convertNumber ( CountString );

  // Validate separatorType is one of the accepted types
  //   and
  // Validate separatorCount is within minimum to maximum length range
  //
  // Set variables if validated and exit program if invalid..
  if ( isValidSeparatorType ( type ) && isValidSeparatorCount ( Count ) ) {

    separatorEnabled = true;
    separatorType = type;
    separatorCount = Count;

  }
  else {

    exit(0);

  }

}

void CHBSPassword::setPad ( std::string Position, std::string Type, std::string CountString ) {

  // Convert padType for validation comparison.
  std::string type = convertType ( Type );

  // Convert padCount for validation comparison.
  int Count = convertNumber ( CountString );

  // Validate padType is one of the accepted types
  //   and
  // Validate padCount is within minimum to maximum length range

  if ( isValidPadType ( type ) && isValidPadCount ( Count ) ) {

    // If the default pad options are not yet overriden, set each enabled to
    // false and then set overriden to true to skip this on future runs to
    // avoid disabling command line options that are already set.
    if ( padDefaultsOverridden == false ) {

      beforeEnabled = false;
      insideEnabled = false;
      afterEnabled = false;

      padDefaultsOverridden = true;

    }

    // Assign correct class variables based on Position.
    if ( Position == "before" ) {

      beforeEnabled = true;
      beforeType = type;
      beforeCount = Count;

    }
    else if ( Position == "inside" ) {

      insideEnabled = true;
      insideType = type;
      insideCount = Count;

    }
    else if ( Position == "after" ) {

      afterEnabled = true;
      afterType = type;
      afterCount = Count;

    }
    else {

      exit(0);

    }

  }
  else {

    exit(0);

  }

}

// END: CHBSPassword PUBLIC

// BEGIN: CHBSPassword PRIVATE

std::string CHBSPassword::getWord() {

  // Select a word from the dictionary based on the minimum and maximum length
  // criteria and then apply capitalization requested.

  std::string word;

  // Variable to indicate that an acceptable length word has been found.
  bool accepted = false;

  while ( accepted != true ) {

    // Select a random word from within the "words" vector.
    word = words[ random() % words.size() ];

    // If the word is within the bounds of the min and max length, set accepted
    // to indicate we have found an acceptable password.
    if ( word.length() >= wordMinimumLength && word.length() <=
        wordMaximumLength ) {

      accepted = true;

    }

    // Apply the requested capitalization.
    word = applyWordCase ( word );

  }

  return word;

}

std::string CHBSPassword::applyWordCase ( std::string Word ) {

  // Called from getWord() this function applies the capitialization requested.

  if ( wordCase == "I" || wordCase == "INITIAL" ) {

    // Set initial letter of each word to be upper case.
    Word[0] = toupper ( Word[0] );

  }
  else if ( wordCase == "U" || wordCase == "UPPER" ) {

    // Set all letters of each word to be upper case.
    std::transform(Word.begin(), Word.end(),Word.begin(), ::toupper);

  }
  else if ( wordCase == "L" || wordCase == "LOWER" ) {

    // Set all letters of each word to be lower case.
    std::transform(Word.begin(), Word.end(),Word.begin(), ::tolower);

  }
  else if ( wordCase == "M" || wordCase == "MIXED" ) {

    // Flip a coin and if heads change letter to be upper case.
    for ( int i = 0; i < Word.length(); i++ ) {

      if ( random() % 2 == 1 ) {

        Word[i] = toupper ( Word[i] );

      }

    }

  }
  else if ( wordCase == "S" || wordCase == "SAME" ) {

    // Do nothing return the word from the dictionary as is.

  }

  return Word;

}

std::string CHBSPassword::applyEleet ( std::string Word ) {

  // Called from getWord() this function applies eleet substitutions if requested.

  return Word;

}

std::string CHBSPassword::getSeparator() {

  // Use separator variables to build and return seperator or returns saved
  // separator if separatorType == SAME.

  std::string separator = "";
  int Count = separatorCount;

  if ( separatorEnabled == true ) {

    // If a separator is already saved for reuse, return it.
    if ( separatorSaved != "" ) {

      return separatorSaved;

    }

    // Loop for the number of separators requested.
    for ( int i = 0; i < Count; i++ ) {

      // If a specific separator is requested, add it.
      if (validSeparators.find( separatorType ) != std::string::npos) {

          separator += separatorType;

      }
      // Else, pick a random one to add.
      else {

          separator += validSeparators[ random() % validSeparators.length() ];

      }

    }

    // If reusing the same separator is requested, save it.
    if ( separatorType == "S" || separatorType == "SAME" ) {

      separatorSaved = separator;

    }

  }

  return separator;

}

std::string CHBSPassword::getPad ( std::string Position ) {

  // Called by get{Before,After,Inside} to use variables to build and return
  // string.

  // Set local variables to value of Position variables
  std::string type = "";
  int Count = 0;

  if ( Position == "before" ) {

    type = beforeType;
    Count = beforeCount;

  }
  else if ( Position == "inside" ) {

    type = insideType;
    Count = insideCount;

  }
  else if ( Position == "after" ) {

    type = afterType;
    Count = afterCount;

  }

  // Set validCharacters to include requested character sets
  std::string validCharacters = "";

  if ( type == "D" || type == "DIGITS" ) {

    // Using Pad of Digits
    validCharacters += validPadDigits;

  }
  else if ( type == "S" || type == "SPECIAL" ) {

    // Using Pad of Special Characters
    validCharacters += validPadSpecialCharacters;

  }
  else if ( type == "M" || type == "MIXED" ) {

    // Using Pad of Mixed Digits and Special Characters
    validCharacters += validPadDigits;
    validCharacters += validPadSpecialCharacters;

  }

  // Build and return randomized pad string

  std::string pad = "";

  for ( int i = 0; i < Count; i++ ) {
    pad += validCharacters[ random() % validCharacters.length() ];
  }

  return pad;
}

std::string CHBSPassword::getBefore() {

  // before - Add string of digits, special characters, or
  // a combination to the beginning of the password.

  return getPad ( "before" );

}

std::string CHBSPassword::getInside() {

  // inside - Add string of digits, special characters, or
  // a combination between the words inside the password.

  return getPad ( "inside" );

}

std::string CHBSPassword::getAfter() {

  // after - Add string of digits, special characters, or
  // a combination to the end of the password.

  return getPad ( "after" );

}

bool CHBSPassword::isValidWordCount ( int wordCount ) {

  // Check to be sure that the word count is 1 or more. Print guiding error
  // message if not.

  if ( wordCount > 0 ) {

    return true;

  }
  else {

    std::cout << "./chbspasswd: unexpected argument for option -- w" << std::endl;
    std::cout << "./chbspasswd: argument must be a number with a value greater than or equal to 0" << std::endl;
    std::cout << std::endl;

    return false;
  }

}

bool CHBSPassword::isValidWordCase ( std::string caseType ) {

  // Check to be sure that the case type requested is one of the valid ones.
  // Print guiding error message if not.

  if ( caseType == "I" || caseType == "INITIAL" ) {

    // Set initial letter of each word to be upper case.
    return true;

  }
  else if ( caseType == "U" || caseType == "UPPER" ) {

    // Set all letters of each word to be upper case.
    return true;

  }
  else if ( caseType == "L" || caseType == "LOWER" ) {

    // Set all letters of each word to be lower case.
    return true;

  }
  else if ( caseType == "M" || caseType == "MIXED" ) {

    // Randomly change each letter to upper or lower case.
    return true;

  }
  else if ( caseType == "S" || caseType == "SAME" ) {

    // Do nothing return the word from the dictionary as is.
    return true;

  }
  else {

    // invalid argument
    std::cout << "./chbspasswd: unexpected argument \"" << caseType << "\" for option -- c" << std::endl;
    std::cout << "./chbspasswd: argument must be either: I (or INITIAL), U (or UPPER)," << std::endl;
    std::cout << "              L (or LOWER), M (or MIXED), or S (or SAME)." << std::endl;
    std::cout << std::endl;

    return false;

  }

}

bool CHBSPassword::isValidSeparatorType ( std::string type ) {

  // Check to be sure that the separator type requested is one of the valid
  // ones. Print guiding error message if not.

  if ( type == "S" || type == "SAME" ) {

    // Will use the same randomly chosen separator throughout.
    return true;

  }
  else if ( type == "R" || type == "RANDOM" ) {

    // Will use a different randomly chosen separator throughout.
    return true;

  }
  // If the type string is not empty, try to find it in the list of valid
  // separators.
  else if ( type != "" && validSeparators.find( type ) != std::string::npos )
  {

    // Will use a specified valid separator throughout.
    return true;

  }
  else {

    // invalid argument
    std::cout << "./chbspasswd: unexpected first argument \"" << type << "\" for option -- s" << std::endl;
    std::cout << "./chbspasswd: argument must be either: S (or SAME), R (or RANDOM)," << std::endl;
    std::cout << "              or one of these valid separators:";
    // Print a list of the valid separators.
    for ( int i = 0; i <= validSeparators.length(); i++ ) {
      std::cout << " " << validSeparators[i] << "";
    }
    std::cout << std::endl;
    std::cout << std::endl;

    return false;

  }

}

bool CHBSPassword::isValidSeparatorCount ( int separatorCount ) {

  // Check to be sure that the separator count requested is 1 or more. Print
  // guiding error message if not.

  if ( separatorCount > 0 ) {

    return true;

  }
  else {

    std::cout << "./chbspasswd: unexpected second argument for option -- s" << std::endl;
    std::cout << "./chbspasswd: argument must be a number with a value greater than or equal to 1" << std::endl;
    std::cout << std::endl;

    return false;

  }

}

bool CHBSPassword::isValidPadType ( std::string padType ) {

  // Check to be sure that the pad type requested is one of the valid
  // ones. Print guiding error message if not.

  if ( padType == "D" || padType == "DIGITS" ) {

    // Using Pad of Digits
    return true;

  }
  else if ( padType == "S" || padType == "SPECIAL" ) {

    // Using Pad of Special Characters
    return true;

  }
  else if ( padType == "M" || padType == "MIXED" ) {

    // Using Pad of Mixed Digits and Special Characters
    return true;

  }
  else {

    // invalid argument
    std::cout << "./chbspasswd: unexpected first argument \"" << padType << "\" for option -- b, i, or a" << std::endl;
    std::cout << "./chbspasswd: argument must be either: D (or DIGITS), S (or SPECIAL)," << std::endl;
    std::cout << "              or M (or MIXED)" << std::endl;
    std::cout << std::endl;

    return false;

  }

}

bool CHBSPassword::isValidPadCount ( int padCount ) {

  // Check to be sure that the separator count requested is 1 or more. Print
  // guiding error message if not.

  if ( padCount > 0 ) {

    return true;

  }
  else {

    std::cout << "./chbspasswd: unexpected second argument for option -- b, i, or a" << std::endl;
    std::cout << "./chbspasswd: argument must be a number with a value greater than or equal to 1" << std::endl;
    std::cout << std::endl;

    return false;

  }

}

std::string CHBSPassword::convertType ( std::string Type ) {

  // Convert string to upper case string for later
  // comparison in validation function.

  std::transform ( Type.begin(), Type.end(), Type.begin(), ::toupper );

  return Type;

}

int CHBSPassword::convertNumber ( std::string numberString ) {

  // Convert a string containing a number into an integer

  int number = atoi ( numberString.c_str() );

  return number;

}

// END: CHBSPassword PRIVATE
