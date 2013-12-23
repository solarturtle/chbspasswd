// chbspasswd v0.1
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

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <GetOpt.h>

class CHBSPassword {

  public:

    CHBSPassword();

    std::string  getPassword();

    std::string  validDigits;
    std::string  validSpecialCharacters;
    std::string  validSeparators;

    std::string  getWord();
    int          wordCount;
    int          wordMinimumLength;
    int          wordMaximumLength;
    std::vector<std::string> buildDictionary();

    std::string  wordCase;

    int          setSeparator();
    std::string  getSeparator();
    bool         separatorEnabled;
    std::string  separatorType;
    int          separatorCount;

    int          setPad ( std::string padPosition, std::string padType, std::string padCount);
    std::string  getPad ( std::string padPosition );
    bool         padDefaultsOverridden;
    int          padMinimumLength;
    int          padMaximumLength;

    static std::string  convertPadType ( std::string padType );
    static int          convertPadCount ( std::string padCount );
    static bool         isValidPadType ( std::string padType );
    static bool         isValidPadCount ( int padCount, int padMinimumLength, int padMaximumLength );

    std::string  getBefore();
    bool         beforeEnabled;
    std::string  beforeType;
    int          beforeCount;

    std::string  getInside();
    bool         insideEnabled;
    std::string  insideType;
    int          insideCount;

    std::string  getAfter();
    bool         afterEnabled;
    std::string  afterType;
    int          afterCount;

    bool         eleetEnabled;

};

int         showHelp();
int         showVersion();
int         showCHBS();

std::vector<std::string> tokenize ( std::string, char );

int main ( int argc, char **argv ) {

  bool DEBUG = true;

  srand( clock() );

  int passwordCount = 1;
  CHBSPassword thisPassword;

  int flag;
  std::vector<std::string> arguments;

  while ( ( flag = getopt ( argc, argv, "a:b:c:ehi:l:n:s:u:vw:x" ) ) != EOF ) {

    if ( flag == 'a' ) {

      // after - Add string of digits, special characters, or a combination to the end of the password.

      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set pad variables
      thisPassword.setPad ( "after", arguments[0], arguments[1] );

    }

    else if ( flag == 'b' ) {

      // before - Add string of digits, special characters, or a combination to the beginning of the password.

      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set pad variables
      thisPassword.setPad ( "before", arguments[0], arguments[1] );

    }

    else if ( flag == 'c' ) {

      // case - Modify the words to be upper, lower, initial, or mixed case 

      std::string caseType = optarg;

      thisPassword.wordCase = caseType;

    }

    else if ( flag == 'e' ) {

      // eleet - Make 1337sp3@k substitutions: a=@, e=3, i=!, l=1, o=0, and t=7.

      thisPassword.eleetEnabled = true;

    }

    else if ( flag == 'h' ) {

      // help - Display a message with usage information 

      showHelp();

      return 0;

    }

    else if ( flag == 'i' ) {

      // inside - Add string of digits, special characters, or a combination between the words inside the password.


      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set pad variables
      thisPassword.setPad ( "inside", arguments[0], arguments[1] );

    }

    else if ( flag == 'l' ) {

      // length - Set minimum and maximum word length.

      arguments = tokenize ( optarg, ',' );

      thisPassword.wordMinimumLength = atoi ( arguments[0].c_str() );
      thisPassword.wordMinimumLength = atoi ( arguments[1].c_str() );

    }

    else if ( flag == 'n' ) {

      // number - Set number of passwords to create.

      std::string count = optarg;

      passwordCount = atoi(count.c_str());

      if ( passwordCount <= 0 ) {

        std::cout << "./chbspasswd: unexpected argument \"" << optarg << "\" for option -- n" << std::endl;
        std::cout << "./chbspasswd: argument must be a number with a value greater than or equal to 1" << std::endl;

        showHelp();

        return -1;
      }

    }

    else if ( flag == 's' ) {

      // seperator - Set the preferences and count of seperator characters between words and other strings.

      arguments = tokenize ( optarg, ',' );

      thisPassword.separatorType = arguments[0];
      thisPassword.separatorCount = atoi(arguments[1].c_str());

    }

// Planned implementation in v0.2
//    else if ( flag == 'u' ) {
//
//      // use - Specify a configuration file to use instead of the default .chbspasswdrc
//
//    }

    else if ( flag == 'v' ) {

      // version - Display a message with version information 

      showVersion();

      return 0;

    }

    else if ( flag == 'w' ) {

      // words - Set the number of words to use.

      std::string count = optarg;

      thisPassword.wordCount = atoi(count.c_str());

    }

    else if ( flag == 'x' ) {

      // xkcd - Override all other options and return a well known password that you have already memorized. ;-)

      showCHBS();

      return 0;

    }

    else {

      showHelp();

      return -1;

    }

  }

  if ( DEBUG ) {
    std::cout << std::endl; 
    std::cout << "passwordCount: " << passwordCount << std::endl; 
    std::cout << std::endl; 
    std::cout << "wordCount: " << thisPassword.wordCount << std::endl; 
    std::cout << "wordMinimumLength: " << thisPassword.wordMinimumLength << std::endl; 
    std::cout << "wordMaximumLength: " << thisPassword.wordMaximumLength << std::endl; 
    std::cout << std::endl; 
    std::cout << "validSeparators: " << thisPassword.validSeparators << std::endl; 
    std::cout << "separatorEnabled: " << thisPassword.separatorEnabled << std::endl; 
    std::cout << "separatorType: " << thisPassword.separatorType << std::endl; 
    std::cout << "separatorCount: " << thisPassword.separatorCount << std::endl; 
    std::cout << std::endl; 
    std::cout << "validDigits: " << thisPassword.validDigits << std::endl; 
    std::cout << "validSpecialCharacters: " << thisPassword.validSpecialCharacters << std::endl; 
    std::cout << "padDefaultsOverridden: " << thisPassword.padDefaultsOverridden << std::endl; 
    std::cout << "padMinimumLength: " << thisPassword.padMinimumLength << std::endl; 
    std::cout << "padMaximumLength: " << thisPassword.padMaximumLength << std::endl; 
    std::cout << std::endl; 
    std::cout << "beforeEnabled: " << thisPassword.beforeEnabled << std::endl; 
    std::cout << "beforeType: " << thisPassword.beforeType << std::endl; 
    std::cout << "beforeCount: " << thisPassword.beforeCount << std::endl; 
    std::cout << std::endl; 
    std::cout << "insideEnabled: " << thisPassword.insideEnabled << std::endl; 
    std::cout << "insideType: " << thisPassword.insideType << std::endl; 
    std::cout << "insideCount: " << thisPassword.insideCount << std::endl; 
    std::cout << std::endl; 
    std::cout << "afterEnabled: " << thisPassword.afterEnabled << std::endl; 
    std::cout << "afterType: " << thisPassword.afterType << std::endl; 
    std::cout << "afterCount: " << thisPassword.afterCount << std::endl; 
    std::cout << std::endl; 
    std::cout << "eleetEnabled: " << thisPassword.eleetEnabled << std::endl; 
    std::cout << std::endl; 
  }

  for ( int i = 1; i <= passwordCount; i++ ) {
    std::cout << thisPassword.getPassword() << std::endl;
  }

}

CHBSPassword::CHBSPassword() {

  // Define valid character types
  validDigits = "0123456789";
  validSpecialCharacters = "!@#$%^&*?";
  validSeparators = "~,.-_=+:";

  // Set defaults for password to be similar to: 5.Cool.Mountain.Africa.$
  wordCount = 3;
  wordMinimumLength = 3;
  wordMaximumLength = 8;

  wordCase = "INITIAL";

  separatorEnabled = true;
  separatorType = "SAME";
  separatorCount = 1;

  padDefaultsOverridden = false;  // This should always be false in the constructor. It will be set to true if _any_ pad option is specified on the commandline b/c if so, all pad options should be disabled and then set to the options specified
  padMinimumLength = 1;
  padMaximumLength = 10;

  beforeEnabled = true;
  beforeType = "DIGITS";
  beforeCount = 1;

  insideEnabled = false;
  insideType = "NONE";
  insideCount = 0;

  afterEnabled = true;
  afterType = "SPECIAL";
  afterCount = 1;

  eleetEnabled = false;

}

std::string CHBSPassword::getPassword() {

  // Build and return password based on the defaults in the configuration file and modifying switches.

  std::string password = "";
  int Count = wordCount;

  // Add a before string if enabled
  if ( beforeEnabled == true ) {
    password += getBefore();
    if ( wordCount >= 1 || afterEnabled == true ) {
      password += getSeparator();
    }
  }

  // If one or zero words are requested the -i option is invalid because an inside string is a separator betweem words.
  if ( Count <= 1 && insideEnabled == true ) {
    std::cout << "unexpected option -- i" << std::endl;
    std::cout << "An INSIDE string is a seperator between WORDS." << std::endl;
    std::cout << "If either one (-w 1) or zero (-w 0) words are requested then -i is invalid." << std::endl;
    return "";
  }

  // While there are more than one words to add, first add a word and then add either a separated inside string if enabled or just a seperator
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

  // If there is only one word is requested, or if there is only one left, just add a word.
  if ( Count == 1 ) {
    password += getWord();
  }

  // Add a before string if enabled
  if ( afterEnabled ) {
    if ( wordCount >= 1 ) {
      password += getSeparator();
    }
    password += getAfter();
  }

  return password;

}

std::string CHBSPassword::getWord() {

  // Select a word from the dictionary based on the minimum and maximum length criteria.

  std::string word;

  std::vector<std::string> words;

  words = buildDictionary();

  bool accepted = false;

  while ( accepted != true ) {

    word = words[ rand() % words.size() ];

    if ( word.length() >= wordMinimumLength && word.length() <= wordMaximumLength ) {

      accepted = true;

    }
  }

  return word;

}

std::vector<std::string> CHBSPassword::buildDictionary () {

  // This string has been manually copied from dictionary.txt
  // It is NOT loaded dynamically. I tried to use a #include, but I couldn't get it to work.
  // If there is a better way to do this, please let me know: charles@solarturtle.net

  std::string dictionary = "a,ability,able,aboard,about,above,accept,accident,according,account,accurate,acres,across,act,action,active,activity,actual,actually,add,addition,additional,adjective,adult,adventure,advice,affect,afraid,Africa,after,afternoon,again,against,age,ago,agree,ahead,aid,air,airplane,Alaska,Alice,alike,alive,all,allow,almost,alone,along,aloud,alphabet,already,also,although,am,America,among,amount,an,ancient,and,Andy,angle,angry,animal,Ann,announced,another,answer,ants,any,anybody,anyone,anything,anyway,anywhere,apart,apartment,appearance,apple,applied,appropriate,April,are,area,arm,army,around,arrange,arrangement,arrive,arrow,art,article,as,Asia,aside,ask,asleep,at,ate,Atlantic,atmosphere,atom,atomic,attached,attack,attempt,attention,audience,August,Aunt,Australia,author,automobile,Autumn,available,average,avoid,aware,away,baby,back,bad,badly,bag,balance,ball,balloon,band,bank,bar,bare,bark,barn,base,baseball,basic,basis,basket,bat,battle,Bay,be,bean,bear,beat,beautiful,beauty,became,because,become,becoming,bee,been,before,began,beginning,begun,behavior,behind,being,believed,bell,belong,below,belt,Ben,bend,beneath,bent,beside,best,bet,Betsy,better,between,beyond,bicycle,bigger,biggest,Bill,Billy,birds,birth,birthday,bit,bite,black,blank,blanket,blew,blind,block,blood,blow,blue,board,boat,Bob,body,bone,book,border,born,both,bottle,bottom,bound,bow,bowl,box,boy,brain,branch,brass,brave,bread,break,breakfast,breath,breathe,breathing,breeze,brick,bridge,brief,bright,bring,British,broad,broke,broken,brother,brought,brown,brush,buffalo,build,building,built,buried,burn,burst,bus,bush,business,busy,but,butter,buy,by,cabin,cage,cake,California,call,calm,came,camera,camp,can,Canada,canal,cannot,cap,capital,captain,captured,car,carbon,card,care,careful,carefully,Carlos,carried,carry,case,Casey,cast,castle,cat,catch,cattle,caught,cause,cave,cell,cent,center,central,century,certain,certainly,chain,chair,chamber,chance,change,changing,chapter,character,characteristic,charge,Charles,chart,check,cheese,chemical,chest,Chicago,chicken,chief,child,children,China,Chinese,choice,choose,chose,chosen,Christmas,church,circle,circus,citizen,city,class,classroom,claws,clay,clean,clear,clearly,climate,climb,clock,close,closely,closer,cloth,clothes,clothing,cloud,club,coach,coal,coast,coat,coffee,cold,collect,college,colony,color,Columbus,column,combination,combine,come,comfortable,coming,command,common,community,company,compare,compass,complete,completely,complex,composed,composition,compound,concerned,condition,congress,connected,consider,consist,consonant,constantly,construction,contain,continent,continued,contrast,control,conversation,cook,cookies,cool,copper,copy,corn,corner,correct,correctly,cost,cotton,could,count,country,couple,courage,course,court,cover,cow,cowboy,crack,cream,create,creature,crew,crop,cross,crowd,cry,cup,curious,current,curve,customs,cut,cutting,Dad,daily,damage,Dan,dance,danger,dangerous,Daniel,Danny,dark,darkness,date,daughter,David,dawn,day,dead,deal,dear,death,decide,declared,deep,deeply,deer,definition,degree,depend,depth,describe,desert,design,desk,detail,determine,develop,development,diagram,diameter,Dick,did,die,differ,difference,different,difficult,difficulty,dig,dinner,direct,direction,directly,dirt,dirty,disappear,discover,discovery,discuss,discussion,disease,dish,distance,distant,divide,division,do,doctor,does,dog,doing,doll,dollar,Don,done,donkey,door,dot,double,doubt,down,dozen,draw,drawn,dream,dress,drew,dried,drink,drive,driven,driver,driving,drop,dropped,drove,dry,duck,due,dug,dull,during,dust,Dutch,duty,each,eager,ear,earlier,early,earn,earth,easier,easily,east,easy,eat,eaten,Eddy,edge,education,Edward,effect,effort,egg,Egypt,eight,either,electric,electricity,element,elephant,eleven,Ellen,else,empty,end,enemy,energy,engine,engineer,England,enjoy,enough,enter,entire,entirely,environment,equal,equally,equator,equipment,escape,especially,essential,establish,Europe,European,even,evening,event,eventually,ever,every,everybody,everyone,everything,everywhere,evidence,exact,exactly,examine,example,excellent,except,exchange,excited,excitement,exciting,exclaimed,exercise,exist,expect,experience,experiment,explain,explanation,explore,express,expression,extra,eye,face,facing,fact,factor,factory,failed,fair,fairly,fall,fallen,familiar,family,famous,far,farm,farmer,farther,fast,fastened,faster,fat,father,favorite,fear,feathers,feature,fed,feed,feel,feet,fell,fellow,felt,fence,few,fewer,field,fierce,fifteen,fifth,fifty,fight,fighting,figure,fill,film,final,finally,find,fine,finest,finger,finish,fire,fireplace,firm,first,fish,five,fix,flag,flame,flat,flew,flies,flight,floating,floor,Florida,flow,flower,fly,fog,folks,follow,food,foot,football,for,force,foreign,forest,forget,forgot,forgotten,form,former,fort,forth,forty,forward,fought,found,four,fourth,fox,frame,France,Frank,Fred,free,freedom,French,frequently,fresh,friend,friendly,frighten,frog,from,front,frozen,fruit,fuel,full,fully,fun,function,funny,fur,furniture,further,future,gain,game,garage,garden,gas,gasoline,gate,gather,gave,general,generally,gentle,gently,George,German,Germany,get,getting,giant,gift,girl,give,given,giving,glad,glass,globe,go,goes,gold,golden,gone,good,goose,got,government,grabbed,grade,gradually,grain,grandfather,grandmother,graph,grass,gravity,gray,great,greater,greatest,greatly,Greece,Greek,green,grew,ground,group,grow,grown,growth,guard,guess,guide,gulf,gun,habit,had,hair,half,halfway,hall,hand,handle,handsome,hang,happen,happened,happily,happy,harbor,hard,harder,hardly,Harry,has,hat,have,having,hay,he,headed,heading,health,heard,hearing,heart,heat,heavy,height,held,hello,help,helpful,Henry,her,herd,here,herself,hidden,hide,high,higher,highest,highway,hill,him,himself,his,history,hit,hold,hole,hollow,home,honor,hope,horn,horse,hospital,hot,hour,house,how,however,huge,human,hundred,hung,hungry,hunt,hunter,hurried,hurry,hurt,husband,I,ice,idea,identity,if,ill,Illinois,image,imagine,immediately,importance,important,impossible,improve,in,inch,include,including,income,increase,indeed,independent,India,Indian,indicate,individual,industrial,industry,influence,information,inside,instance,instant,instead,instrument,interest,interior,into,introduced,invented,involved,iron,is,island,it,Italian,Italy,its,itself,jack,James,Jane,January,Japan,Japanese,jar,Jeff,jet,Jim,Jimmy,job,Joe,John,Johnny,Johnson,join,joined,Jones,journey,joy,judge,July,jump,June,jungle,just,keep,kept,key,kids,kill,kind,King,kitchen,knew,knife,know,knowledge,known,label,labor,lack,lady,laid,lake,lamp,land,language,large,larger,largest,last,late,later,Latin,laugh,law,lay,layers,lead,leader,leaf,learn,least,leather,leave,leaving,led,Lee,left,leg,length,lesson,let,letter,level,library,lie,life,lift,light,like,likely,limited,Lincoln,line,lion,lips,liquid,list,listen,little,live,living,load,local,locate,location,log,London,lonely,long,longer,look,loose,lose,loss,lost,lot,loud,Louis,love,lovely,low,lower,luck,lucky,lunch,lungs,lying,machine,machinery,mad,made,magic,magnet,mail,main,mainly,major,make,making,Mama,man,managed,manner,manufacturing,many,map,March,Maria,Mark,market,married,Mars,Martin,Mary,mass,massage,master,material,mathematics,matter,May,maybe,me,meal,mean,means,meant,measure,meat,medicine,meet,melted,member,memory,men,mental,merely,met,metal,method,Mexico,mice,middle,might,mighty,Mike,mile,military,milk,mill,mind,mine,minerals,minute,mirror,Miss,missing,mission,Mississippi,mistake,mix,mixture,model,modern,molecular,moment,money,monkey,month,mood,moon,more,morning,most,mostly,mother,motion,motor,Mount,mountain,mouse,mouth,move,movement,movie,moving,mud,muscle,music,musical,must,my,myself,mysterious,nails,name,nation,national,native,natural,naturally,nature,near,nearby,nearer,nearest,nearly,necessary,neck,needed,needle,needs,negative,Negro,neighbor,neighborhood,nervous,nest,never,new,news,newspaper,next,nice,night,nine,no,nobody,nodded,noise,none,noon,nor,north,Norway,nose,not,note,noted,nothing,notice,noun,now,number,numeral,nuts,object,observe,obtain,occasionally,occur,ocean,October,of,off,offer,office,officer,official,Ohio,oil,old,older,oldest,on,once,one,only,onto,open,operation,opinion,opportunity,opposite,or,orange,orbit,order,ordinary,organization,organized,origin,original,other,ought,our,ourselves,out,outer,outline,outside,over,own,owner,oxygen,Pacific,pack,package,page,paid,pain,paint,pair,palace,pale,pan,Papa,paper,paragraph,parallel,parent,Paris,park,part,particles,particular,particularly,partly,parts,party,pass,passage,past,path,pattern,Paul,pay,peace,pen,pencil,Pennsylvania,people,per,percent,perfect,perfectly,perhaps,period,person,personal,pet,Peter,Philadelphia,phrase,physical,piano,pick,picture,pictured,pie,piece,pig,pile,pilot,pine,pink,pipe,pitch,place,plain,plan,plane,planet,planned,planning,plant,plastic,plate,plates,play,pleasant,please,pleasure,plenty,plural,plus,pocket,poem,poet,poetry,point,Pole,police,policeman,political,pond,pony,pool,poor,popular,population,porch,port,position,positive,possible,possibly,post,pot,potatoes,pound,pour,powder,power,powerful,practical,practice,prepare,present,president,press,pressure,pretty,prevent,previous,price,pride,primitive,principal,principle,printed,private,prize,probably,problem,process,produce,product,production,program,progress,promised,proper,properly,property,protection,proud,prove,provide,public,pull,pupil,pure,purple,purpose,push,put,putting,quarter,queen,question,quick,quickly,quiet,quietly,quite,rabbit,race,radio,railroad,rain,raise,ran,ranch,range,rapidly,rate,rather,raw,rays,reach,read,reader,ready,real,realize,rear,reason,recall,receive,recent,recently,recognize,record,red,refer,refused,region,regular,related,relationship,religious,remain,remarkable,remember,remove,repeat,replace,replied,report,represent,require,research,respect,rest,result,return,review,rhyme,rhythm,rice,rich,Richard,ride,riding,right,ring,rise,rising,river,road,roar,Robert,rock,rocket,rocky,rod,roll,Roman,Rome,roof,room,root,rope,rose,rough,round,route,row,rubbed,rubber,rule,ruler,run,running,rush,Russia,Russian,sad,saddle,safe,safety,said,sail,sale,Sally,salmon,salt,Sam,same,sand,sang,sat,satellites,satisfied,Saturday,save,saved,saw,say,scale,scared,scene,school,science,scientific,scientist,score,screen,sea,search,season,seat,second,secret,section,see,seed,seeing,seems,seen,seldom,select,selection,sell,send,sense,sent,sentence,separate,series,serious,serve,service,sets,setting,settle,settlers,seven,several,shade,shadow,shake,shaking,shall,shallow,shape,share,sharp,she,sheep,sheet,shelf,shells,shelter,shine,shinning,ship,shirt,shoe,shoot,shop,shore,short,shorter,shot,should,shoulder,shout,show,shown,shut,sick,sides,sight,sign,signal,silence,silent,silk,silly,silver,similar,simple,simplest,simply,since,sing,single,sink,Sir,sister,sit,sitting,situation,six,size,skill,skin,sky,slabs,slave,sleep,slept,slide,slight,slightly,slip,slipped,slope,slow,slowly,small,smaller,smallest,smell,smile,Smith,smoke,smooth,snake,snow,so,soap,social,society,soft,softly,soil,solar,sold,soldier,solid,solution,solve,some,somebody,somehow,someone,something,sometime,somewhere,son,song,soon,sort,sound,source,south,southern,space,Spain,speak,special,species,specific,speech,speed,spell,spend,spent,spider,spin,spirit,spite,split,spoken,sport,spread,spring,square,stage,stairs,stand,standard,star,stared,start,state,statement,States,station,stay,steady,steam,steel,steep,stems,step,stepped,stick,stiff,still,stock,stomach,stone,stood,stop,stopped,store,storm,story,stove,straight,strange,stranger,straw,stream,street,strength,stretch,strike,string,strip,strong,stronger,struck,structure,struggle,stuck,student,studied,studying,subject,substance,success,successful,such,sudden,suddenly,sugar,suggest,suit,sum,summer,sun,Sunday,sunlight,supper,supply,support,suppose,sure,surface,surprise,surrounded,swam,sweet,swept,swim,swimming,swing,swung,syllable,symbol,system,table,tail,take,taken,tales,talk,tall,tank,tape,task,taste,taught,tax,tea,teach,teacher,team,tears,teeth,telephone,television,tell,temperature,ten,tent,term,terrible,test,Texas,than,thank,that,the,thee,them,themselves,then,theory,there,therefore,these,they,thick,thin,thing,think,third,thirty,this,Thomas,those,thou,though,thought,thousand,thread,three,threw,throat,through,throughout,throw,thrown,thumb,thus,thy,tide,tie,tight,tightly,till,Tim,time,tin,tiny,tip,tired,title,to,tobacco,today,together,told,Tom,tomorrow,tone,tongue,tonight,too,took,tool,top,topic,torn,total,touch,toward,tower,town,toy,trace,track,trade,traffic,trail,train,transportation,trap,travel,treated,tree,triangle,tribe,trick,tried,trip,troops,tropical,trouble,truck,trunk,truth,try,tube,tune,turn,TV,twelve,twenty,twice,two,type,typical,uncle,under,underline,understanding,unhappy,union,unit,United,universe,University,unknown,unless,until,unusual,up,upon,upper,upward,us,use,useful,using,usual,usually,valley,valuable,value,vapor,variety,various,vast,vegetable,verb,vertical,very,vessels,victory,view,village,Virginia,visit,visitor,voice,volume,vote,vowel,voyage,wagon,wait,walk,wall,want,war,warm,warn,was,wash,Washington,waste,watch,water,wave,way,we,weak,wealth,wear,weather,week,weigh,weight,welcome,well,went,were,west,western,wet,whale,what,whatever,wheat,wheel,when,whenever,where,wherever,whether,which,while,whispered,whistle,white,who,whole,whom,whose,why,wide,widely,wife,wild,will,William,willing,Wilson,win,wind,window,wing,winter,wire,wise,wish,with,within,without,wolf,women,won,wonder,wonderful,wood,wooden,wool,word,wore,work,worker,world,worried,worry,worse,worth,would,wrapped,write,writer,writing,written,wrong,wrote,yard,year,yellow,yes,yesterday,yet,you,young,younger,your,yourself,youth,zero,zoo";

  std::istringstream iss ( dictionary );

  std::string word;
  std::vector<std::string> words;

  while ( std::getline ( iss, word, ',' ) ) {
    words.push_back(word);
  }

  // Returns the vector.
  return words;

}

int CHBSPassword::setSeparator() {

  // Set separator variables

}

std::string CHBSPassword::getSeparator() {

  // Use separator variables to build and return seperator or returns saved separator if separatorType == SAME.

  // Temporary return to get getPassword moving early

  std::string separator = ".";

  return separator;

}

int CHBSPassword::setPad ( std::string padPosition, std::string padType, std::string padCount ) {

  // Convert padType for validation comparison.
  std::string type = CHBSPassword::convertPadType ( padType );

  // Convert padCount for validation comparison.
  int Count = CHBSPassword::convertPadCount ( padCount );

  // Validate padType is one of the accepted types
  //   and
  // Validate padCount is within minimum to maximum length range

  if ( CHBSPassword::isValidPadType ( type )
         &&
       CHBSPassword::isValidPadCount ( Count, padMinimumLength, padMaximumLength ) ) {

    // If the default pad options are not yet overriden, set each enabled to false and then set overriden to true to skip this on future runs to avoid disabling command line options that are already set.
    if ( padDefaultsOverridden == false ) {

      beforeEnabled = false;
      insideEnabled = false;
      afterEnabled = false;

      padDefaultsOverridden = true;

    }

    // Assign correct class variables based on padPosition.
    if ( padPosition == "before" ) {

      beforeEnabled = true;
      beforeType = type;
      beforeCount = Count;

    }
    else if ( padPosition == "inside" ) {

      insideEnabled = true;
      insideType = type;
      insideCount = Count;

    }
    else if ( padPosition == "after" ) {

      afterEnabled = true;
      afterType = type;
      afterCount = Count;

    }
    else {

      return -1;

    }

  }
  else {

    return -1;

  }

  return 0;

}

std::string CHBSPassword::getPad ( std::string padPosition ) {

  // Called by get{Before,After,Inside} to use variables to build and return string.

  // Set local variables to value of padPosition variables

  std::string type = "";
  int Count = 0;

  if ( padPosition == "before" ) {

    type = beforeType;
    Count = beforeCount;

  }
  else if ( padPosition == "inside" ) {

    type = insideType;
    Count = insideCount;

  }
  else if ( padPosition == "after" ) {

    type = afterType;
    Count = afterCount;

  }
 
  // Set validCharacters to include requested character sets

  std::string validCharacters = "";

  if ( type == "D" || type == "DIGITS" ) {

    // Using Pad of Digits

    validCharacters += validDigits;

  }
  else if ( type == "S" || type == "SPECIAL" ) {

    // Using Pad of Special Characters

    validCharacters += validSpecialCharacters;

  }
  else if ( type == "M" || type == "MIXED" ) {

    // Using Pad of Mixed Digits and Special Characters

    validCharacters += validDigits;
    validCharacters += validSpecialCharacters;

  }

  // Build and return randomized pad string

  std::string pad = "";

  for ( int i = 0; i < Count; i++ ) {
    pad += validCharacters[ rand() % validCharacters.length() ];
  }

  return pad;
}

std::string CHBSPassword::convertPadType ( std::string padType ) {

  // Convert string padType to upper case string for later comparison in validation function.

  std::transform ( padType.begin(), padType.end(), padType.begin(), ::toupper );

  return padType;

}

int CHBSPassword::convertPadCount ( std::string padCount ) {

  // Convert string padCount to int Count.

  int Count = atoi ( padCount.c_str() );

  return Count;

}

bool CHBSPassword::isValidPadType ( std::string padType ) {

  if ( padType == "D" || padType == "DIGITS" ) {
    // Using Pad of Digits

    return 1;
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
    std::cout << "./chbspasswd: unexpected argument -- " << padType << std::endl;
    showHelp();

    return false;
  }

}

bool CHBSPassword::isValidPadCount ( int padCount, int padMinimumLength, int padMaximumLength ) {

  if ( padCount <= padMaximumLength ) {
    return true;
  }
  else {
    return false;
  }

}

std::string CHBSPassword::getBefore() {

  // Return string for ...
  // before - Add string of digits, special characters, or a combination to the beginning of the password.

  return getPad ( "before" );

}

std::string CHBSPassword::getInside() {

  // Return string for ...
  // inside - Add string of digits, special characters, or a combination between the words inside the password.

  return getPad ( "inside" );

}

std::string CHBSPassword::getAfter() {

  // Return string for ...
  // after - Add string of digits, special characters, or a combination to the end of the password.

  return getPad ( "after" );

}

int showHelp() {

  // help - Display a message with usage information 

  std::cout << std::endl;
  std::cout << "Usage: chbspasswd [ -w numberOfWords ]" << std::endl;
  std::cout << "                  [ -l minimumWordLength,maximumWordLength ]" << std::endl;
  std::cout << "                  [ -c {UPPER|LOWER|INITIAL|MIXED} ]" << std::endl;
  std::cout << "                  [ -s typeOfSeparator,count ]" << std::endl;
  std::cout << "                  [ -{a|b|c} {DIGITS|SPECIAL|MIXED},count ]" << std::endl;
  std::cout << "                  [ -n numberOfPasswordsToGenerate ]" << std::endl;
  std::cout << std::endl;
  std::cout << "For more detailed information: man chbspasswd" << std::endl;
  std::cout << std::endl;

  return 0;

}

int showVersion() {

  // version - Display a message with version information 

  std::cout << "chbspasswd v0.1" << std::endl;

  return 0;

}

int showCHBS() {

  // xkcd - Override all other options and return a well known password that you have already memorized. ;-)

  std::cout << "CorrectHorseBatteryStaple" << std::endl;

  return 0;

}

std::vector<std::string> tokenize ( std::string delimiterSeparatedString, char delimiter ) {

  // Accepts a delimiter separated string and the specified delimiter.

  std::istringstream iss ( delimiterSeparatedString );

  // Tokenizes the string and puts the tokens into a vector

  std::string token;
  std::vector<std::string> tokens;

  while ( std::getline ( iss, token, delimiter ) ) {
    tokens.push_back(token);
  }

  // Returns the vector.
  return tokens;

}

