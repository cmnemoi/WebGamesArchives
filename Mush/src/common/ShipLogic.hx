import com.Goal;
import db.AdminStream;
import db.Hero;
import db.Ship;
import db.Triumph;
import db.User;
import db.Patrol;
import db.ShipLog;
import db.Hunter;
import db.PNJ;
import haxe.CallStack;
import neko.Web;

import haxe.PosInfos;
import haxe.Serializer;
import haxe.Unserializer;
import haxe.EnumFlags;

import data.Level;
import data.Map;

import sys.db.Types;
import Types;

import Protocol;

using Ex;
using Logic;
using Utils;
using ItemUtils;

import HashEx;
import IntHashEx;

import mt.gx.Pair;

typedef HunterIaCache  =
{
	var suits : List<Hero>;
	var patrols : List<Patrol>;
	var externEquip : List < ItemId>;
	var hunters : List<Hunter>;
}

class ShipLogic
{
	var ship : db.Ship;
	public function new(ship : db.Ship)
	{
		this.ship =  ship;
	}
	
	public function embarkGroupMember( hero : Hero , force = false )
	{
		if ( !force )
		{
			mt.gx.Debug.assert( ship.acceptsPlayers );
			mt.gx.Debug.assert( hero.getHeroId() != ADMIN );
		}
		
		for( i in PAColors.array() )
			hero.currentPa[ i.index() ] = 0;
		
		ActionLogic.clearIterEffectsCache(hero.ship);

		hero.basePaSetup();
		
		if ( hero.currentPa[GEN.index()] == 0)
			throw "ASSERT PA";
			
		if( hero.heroId != ADMIN )
			ship.crewLength++;
			
		ship.recache();
		
		ShipLog.createHeroLog( EventId.NEW_CREW_MEMBER,  hero ).insert();
		
		var user = hero.getOwner( false );
		user.onNewGame();
		
		var stats = user.stats( true );
		stats.nbGameStarted++;
		stats.update();
		
		if ( ship.crewLength >= ship.getSeasonData().maxCrew )
			ship.gameStart();
		
		hero.update();
		ship.update();
		
		ship.groupScript( hero );
		
		if ( ship.conf.isSeasonExhausted == true) {
			if ( Dice.percent(10) ) {
				hero.hp -= 1;
			}
			else if ( Dice.percent(10) ) {
				hero.hp -= 2;
			}
			else if ( Dice.percent(50) ){
				var table = [
					{weight:10,id:EXPLODED_NOSE},
					{weight:10,id:BROKEN_FINGER},
					
					{weight:10,id:BRUISED_SHOULDER},
					{weight:1,id:BROKEN_RIBS},
					{weight:1,id:BROKEN_FINGER},
				];
				if ( hero.heroId != TERRENCE_ARCHER) {
					table.push( { weight:5, id:BROKEN_FOOT } );
				}
				var widx = mt.gx.ArrayEx.normRand( table );
				var w = table[widx].id;
				hero.wound(w);
			}
			
		}
		
		if ( ship.conf.isSeasonHungry == true && !hero.isTuto() ) {
			if ( Dice.percent(50) )
				hero.nurture = Const.DEATH_NURTURE;
		}
		
		
	}

	public function spawnLearnBook()
	{
		var skillIdx = RandomEx.normalizedRandom( ship.data.skillTab );
		if ( skillIdx >= ship.data.skillTab.length )
		{
			App.current.logError("ARGH skills " + ship.data.skillTab,haxe.CallStack.toString(haxe.CallStack.callStack()));
			return;
		}
		
		var data = ship.data.skillTab[skillIdx];
		if ( data == null) return;
		ship.data.skillTab.splice( skillIdx, 1);
		
		var bk = Utils.itemDesc( ship, BOOK );
		bk.customInfos.push(  Skilled(data.id) );
		bk.customInfos.push(  Book(LEARN_BOOK) );
		
		var store = ship.getRoomsByType(STORAGE)
		.random();
		
		Debug.ASSERT( store != null );
		
		store.inventory.push( bk );
		ship.dirty = true;
		//new AdminStream( ship, "lambda book created " + Std.string( bk ) );
	}
	
	//push infos into log and kill the peon
	public static function killMember( hero: Hero, cause : DeathId)
	{
		if ( hero == null ) return;
		
		var mTrace = function(s)
		{
			//ShipLog.createShipLog( CHAT, hero.ship, ChatEntry( s )).set(Admin).insert();
		};
		
		mTrace("trying to kill " +hero.getHeroId() );
		if( hero.isDead())
		{
			mTrace("already Dead");
			return;
		}
		
		mTrace("retrieving owner");
		var user = hero.getOwner(true);
		var ship = hero.ship;
		var hid = hero.heroId;
		var isRealDeath = HeroLogic.isDeathCause(cause );
		
		if( !ship.isLocked()) ship.lock();
		
		mt.gx.Debug.assert( hero.isLocked());
		mt.gx.Debug.assert( hero.ship.isLocked());
		
		if ( hero.isMush() && isRealDeath)
		{
			ship.data.mushDead = true;
			var h = ship.searchTpl( GIOELE_RINALDO,true );
			if ( h != null && !h.isMush() && !h.isDead()) h.triumph( MUSH_FEAR );
		}
		
		if ( isRealDeath )
			if ( hero.isParia() )
			{
				var juge = Hero.manager.get( hero.anathemer, true );
				var tval = Protocol.gloryDb( TR_ANATHEM );
				
				var shallConsider = !juge.isMush() && ( !juge.isDead() || juge.deathCycle != ship.currentCycle );
				if ( shallConsider )
				{
					if ( hero.isMush())		juge.triumph( TR_ANATHEM,PARIA_TRIUMPH, tval.score);
					else					juge.triumph( TR_ANATHEM,PARIA_TRIUMPH, -tval.score);
				}
			}	
					
		
		//planet tag orphanage
		if( hero.isLost() && ship.getPlanet() != null)
		{
			var pl = ship.getPlanet();
			var found = false;
			for ( plt in pl.data.tags)
				if ( plt.tg == LOST &&	plt.ts == TS_Known)
				{
					found = true;
					plt.ts = TS_Explored;
					break;
				}
			ship.dirty = true;
		}
		
		switch(cause) {
			default:
			case DC_ABDUCTED_BY_ALIENS:
				hero.triumph(ALIEN_SCIENCE);
		}
		
		mTrace("set to dead");
		hero.touch(false);
		hero.flags.set(DEAD);
		hero.dead = true;
		hero.tutoState = null;
		
		if( isRealDeath)
			if ( hero.heroId == ZHONG_CHUN)
				for (h in ship.liveHeroes(true).filter(function(h) return h.isMush()))
					h.triumph(CHUN_DEAD);
		
		var lh = ship.liveHeroes( false );
		if ( lh.length == 0 && isRealDeath)
		{
			if ( hero.titles.has( COMMANDER ) )	hero.goal( com.Goal.id.commander_should_go_last );
			hero.goal( com.Goal.id.last_member );
		}
		
		hero.deathCycle = hero.ship.currentCycle;
		mTrace("dropping items");
		
		for( i in hero.inventory )
			ActionLogic.dropItem( hero, i );
			
		hero.inventory.clear();
		
		var isCryo = (cause == DC_CRYOGENIZED) || (cause == DC_SELF_CRYOGENIZED);
		
		mTrace("evaluating cryo conds");
		
		if ( isRealDeath )
		{
			ShipLog.createHeroLog( DEATH, hero , Death(cause)).insert();
			
			var show = ship.neronVal( NB_ADVERTISE_DEATHS ) == 0;
			
			var msg = function(evt) return ShipLog.createHeroLog( evt, hero , Death(cause)).set(FakeNeron).setRid(null);
			
			var l = switch(hero.heroId) {
				case JANICE_KENT:			msg( NERON_HERO_DEATH_JANICE);
				case RALUCA_TOMESCU	:		msg( NERON_HERO_DEATH_RALUCA);
				
				default:{
					if( isRealDeath)
						switch(cause)
						{
							case DeathId.DC_OXYGEN: msg( OXY_LOW_DAMMIT);
							default: 				msg( NERON_HERO_DEATH);
						}
					else 
						switch(cause)
						{
							case DC_ABDUCTED_BY_ALIENS:
								msg(NERON_HERO_DISAPEAR); 
							default: msg(NERON_HERO_DEATH);
						}
				}
			}
			if ( !show ) l.set(Admin);
			l.insert();
		}
		
		ShipLog.createHeroLog( DEATH, hero , Death(cause)).set(Admin).insert();
		
		var heroes =  hero.ship.liveHeroes( true );
		
		mTrace("checking for psy traumas");
		
		var v = 1;
		if ( hero.isPregnant())
			v++;
		
		if ( 	cause != DC_SUICIDE 
		&& 		!isCryo 
		&& 		isRealDeath 
		&& 		cause != DC_MODERATED) // dont allow suicide chains
		{
			for (x in heroes )
				if (  !x.skillsHas( SkillId.UNCONCERNED) )
				{
					x.decrMoral( v );
					if( x.location == hero.location )
						HeroLogic.tryPsyTrauma(x);
				}
				
			ship.cmTriumph(CM_ALL_MUSH_HUMANICIDE);
		}
		
		if ( isRealDeath && !isCryo)
			for (x in heroes )
				if ( x.skillsHas(COLD_BLOOD))
				{
					ShipLog.createPersoLog( SKILL_ADD_PA, x, SL_Skill( COLD_BLOOD )).insert();
					x.addPa( GEN, 3 );
				}
		
		for (x in heroes )
			x.update();
		
		var last = heroes.filter(function(h) return !h.isDead());
		
		mTrace("try processing user");
		if ( user != null)
		{
			mTrace("starting user update");
		
			if( cause == DC_SELF_CRYOGENIZED )
				user.lastSelfCryo = Date.now();
		
			mTrace("historizing");
			
			var hs : db.HistoryHero.ShipResume =
			{
				userId:user.id,
				shipId: ship.id,
				deathCycle: hero.deathCycle - ship.getStartingCycle(),
				charId: hero.charTplId,
				deathId: cause,
				checked:false,
				heroId : hero.id,
				deathLocation :  hero.location,
				wasMush: hero.isMush(),
				
				titles:
					{
						var l = [];
						for ( t in TitlesId.array())
							if ( hero.titles.has( t ) )
								l.pushBack( t.index() );
						l;
					},
				skills: new FlagsArray(SkillId).readArray( hero.skills ).readArray(hero.oldSkills),
				epitaph:null,
				gameLog: hero.buildStory(),
			};
			
			//user.history.pushBack(hs);
			var hh = new db.HistoryHero( ship, hs,user );
			hh.insert();
		}
		hero.titles = EnumFlags.ofInt(0);
			
		mTrace("faking for cryo");
		
		mTrace("pushing to limbo");
		hero.location = RoomId.LIMBO;
		hero.dead = true;
		
		mTrace("updating ship hero");
		hero.update();
		hero.ship.update();
		
		mTrace("updating user");
		user.onDeath();
		user.update();
				
		new db.AdminStream( ship, "killed "+ hid);
		
		if ( last.length <= 0 && !ship.acceptsPlayers )
		{
			if( !ship.destroyed)
			{
				mTrace("destroying ship");
				new ShipLogic(hero.ship).destroyShip();
			}
		}
		
	}
	
	public static function checkCommMaster(ship:db.Ship)
	{
		if ( ship.data.neronVer < 500)
			return;
		
		for ( r in ship.data.com.rebelData )
			if ( !r.decoded ) return;
		
		for ( x in ship.getXyloph() )
			if ( !ship.data.com.xylophDb.has( x.id ) ) return;
		
		if ( !ship.flags.has( COMM_MASTER ) ) {
			ship.goal( com.Goal.id.communicator );
			ship.flags.set(COMM_MASTER);
			ship.dirty = true;
		}
		
		
	}
	
	public static function lookupRschInfos(hero : Hero, ship:Ship) : { chun:Bool, cryo:Bool}
	{
		var heroes = ship.liveHeroes( false );
		
		var invs :List<ItemDesc> = hero.inventory.concat( hero.locInv() );
		var i = { chun: ship.isXylophUnlocked( ZONG_CHUN_SAMPLE ), cryo:ship.data.mushDead };
		
		if(!i.chun)
		for(h in heroes)
			if ( h.location == hero.location && h.getHeroId() == ZHONG_CHUN )
			{
				i.chun = true;
				break;
			}

		var omap = ship.objectMap();
		for( c in omap)
			if(c.item.id == CRYO_MODULE)
				for ( prm in c.item.customInfos)
				{
					var wasMush = false;
					
					switch( prm )
					{
						case BodyOf( _, b ): wasMush = (b == null) ? false : b;
						default: continue;
					}
					
					if( wasMush )
					{
						i.cryo = true;
						break;
					}
				}
				
		return i;
	}
	
	public static function extractRschList(hero : Hero,ship : Ship) : List<RschInfo>
	{
		var p = LambdaEx.partition( ship.researchList,function(r) return( r.progress < 100 ) );
		var l : List<RschInfo> = p.first;
		var vl : List<RschInfo> = p.second;
		
		var i = lookupRschInfos(hero,ship);
		
		l = Lambda.filter(l,function( r )
		{
			var data = Protocol.researchDb( r.id) ;
			if( data.need_chun && ! i.chun)
				return false;
			
			if(data.need_cryo && !i.cryo)
				return false;
			
			var inv = hero.researchInv().array();
			if ( ship.ruleFlags.has( Rule.NILS_CARLSSON_CONTACT ) )
				inv.push( Utils.itemDesc( ship, MUSH_SAMPLE,true ));
			
			for (s in data.samples)
			{
				var ok = inv.findIndex( function(i) return i.id == s );
				if ( ok!=null )
					inv.splice( ok,1 );
				else
					return false;
			}
			
			return true;
		});
		
		return l;
	}
	
	public static function remapTitles( ship : Ship, force = false )
	{
		if ( !ship.isGameStarted() && !force)
			return;
			
		var heroes = ship.liveHeroes( true );
		
		function canHasTitle(h:db.Hero)
		{
			return !h.flags.has(BERZERK) && !h.flags.has(SLEEPY);
		}
		
		for( t in Protocol.titleList )
		{
			if (0 == t.priority.length) continue;
		
			var isThereOne : Hero = heroes.find( function(h) return h.titles.has( t.id ) );
			var that : Hero = null;
			var prioList = t.priority.copy();
			
			if( t.id == COMMANDER ){
				var putschists = [];
				for ( h in heroes ) 
					if ( h.pFlags.has(PUTSCHIST )) 
						putschists.push( h );
				putschists.sort( function(h0, h1) return Reflect.compare( h0.charTplId, h1.charTplId ));
				for ( p in putschists ){
					prioList.remove( p.heroId );
					prioList.unshift( p.heroId );
				}
			}
			
			//neko.Web.logMessage( t.id + " " + prioList );
			
			if( null == isThereOne)	{
				for(m in prioList) {
					that = heroes.find( function(h) return h.getHeroId() == m  && canHasTitle(h) );
					if (null==that) continue;
					
					ShipLog.createNeronLog(TITLE_MOVE, ship, TitlePass(	t.id, null, that != null ? that.getHeroId() : null) ).insert();
					
					that.grantTitle(t.id);
					break;
				}
			}
			else {
				for(m in prioList) {
					that = heroes.find( function(h) return h.getHeroId() ==  m  && canHasTitle(h) );
					
					if (null == that) continue;
					if ( that == isThereOne ) break;
					
					ShipLog.createNeronLog(TITLE_MOVE, ship, TitlePass(	t.id, isThereOne != null ? isThereOne.getHeroId() : null,
																that != null ? that.getHeroId() : null) ).insert();
																
					isThereOne.titles.unset(t.id);
					isThereOne.update();
					
					that.grantTitle(t.id);
					break;
				}
				isThereOne.tickPM();
			}
		}
	}
	
	
	public static function setFire( ship : Ship ,r : RoomId )
	{
		var rd = ship.roomData.get(r);
		if ( !rd.status.has(FIRE) && !rd.status.has(INDESTRUCTIBLE) )
		{
			ShipLog.createNeronLog(SET_ON_FIRE, ship).insert();
			ship.roomData.get(r).status.set(FIRE);
		}
	}
	
	
	
	// roomIndex 2 heroes
	public static inline function roomOccupation( ship : Ship, r : RoomId) : Int
	{
		return listHeroesInRoom( ship, r , false ).length;
	}
	
	public static function maxOccupation( ship : Ship, r : RoomId )
	{
		var  base = Protocol.roomTypeDb( Protocol.roomType( r ).id ).capacity;
		if( r ==  RoomId.ICARUS  )
			base = Protocol.shipStatsDb( ShipStatsId.ICARUS ).capacity;
			if (ship.isProjectUnlocked(ICARUS_LARGER_BAY))
				base += 2;
		
		return base;
	}
	
	public static function makeCache(ship : Ship) : HunterIaCache
	{
		var heroes = ship.heroes();
		var patrols = ship.cache.patrols.list();
		var cache = [];
		var hts =  ship.cache.hunters;
		
		return
		{
			suits : heroes.filter( function(h) return h.loc().id == RoomId.OUTER_SPACE ),
			patrols : patrols,
			externEquip : 	ship.getRoom(RoomId.OUTER_SPACE).inventory
								.filter( function(i) return i.status.has( EQUIPMENT) && !i.status.has(BROKEN) )
								.map(function(i)return i.id),
			hunters : hts,
		};
	}
	
	public static function ApplyShield( ship : Ship,v : Int ) : Int {
		if ( ship.isProjectUnlocked(ProjectId.PLASMA_SHIELD) && ship.neronVal(NB_ENABLE_PLASMA_SHIELD)==0)
		{
			var old = ship.shield;
			if(ship.shield > v)
			{
				ship.shield = MathEx.clampi( ship.shield - v, 0, 100);
				v = 0;
			}
			else
			{
				var diff = v - ship.shield;
				ship.shield = MathEx.clampi( ship.shield - v, 0, 100);
				v -= diff;
			}
			
			var thresh = 10;
			
			if( ship.shield <= thresh && old > thresh)
				ShipLog.createNeronLog( SHIELD_LOW, ship , Qty(v) ).insert();
		}
		return v;
	}
	
	public static function onHunterNewCycle( h : Hunter , cache : HunterIaCache)
	{
		if ( h.dead ) return;
		
		var ship = h.ship;
		var hd = h.getData();
		var curRoom = ship.getRoom( RoomId.OUTER_SPACE );
		
		var nb = 1;
		switch( ShipStatsId.createI(h.type)){
			default:
			case DICE: nb = 3;
			case TRANSPORT: 
				if ( h.transportData.aggroed == true ) {
					h.tradeFlee();
				}
				return;
			case ASTEROID: 
				h.charge--;
				if( h.charge > 0)
					return;
				else
				{
					ShipLogic.hurtHull( ship, h.hp );
					h.shotDown();
				}
		}
	
		for( i in 0...nb)
			switch(h.state )
			{
			case Stalled: throw "inconsistency";
			case Move://move to n	ext target indeed
				{
					var done;
					var d = Dice.D100();
					var st = cache;
					
					
					if ( ActionLogic.doDataCheckS( ResearchUnlocked(FUNGUS_SCRAMBLER), ship ))
					{
						if(Dice.percent( 20 ))
						{
							var tgt = st.hunters.filter( function(hh) return h.id != hh.id).random();
							if ( tgt != null )
							{
								h.state = LockHunter( tgt.id );
								return;
							}
						}
					}
					
					var tr = st.hunters.filter(function(h) return h.getType() == TRANSPORT );
					
					if ( tr.length > 0 ) 
						if(Dice.percent( 20 )){
							h.state = LockTransport( tr.random().id );
							return;
						}
					
					if( st.patrols.length > 0)
					{
						for(p in st.patrols)
						{
							if ( p.isDocked()) continue;
							
							var heroes = p.heroes(false);
							
							var v = 15;
							var cont = true;
							for( h in heroes)
							{
								var d = Utils.stanceData( h.data.patrolStance );
								if( !d.lock )
								{
									cont = false;
									break;
								}
								v = MathEx.maxi( v, d.attract );
							}
							
							if(!cont)
							{
								break;
							}
							
							if( Dice.percent(v))
							{
								h.state = LockPatrol( RoomId.createI( p.rid ) );
								return;
							}
						}
					}
					
					if (st.suits.length > 0 )
					{
						for(hr in st.suits)
						{
							if( Dice.percent(5) )
							{
								var hr:Hero = st.suits.random();
								h.state = LockHuman( hr.getHeroId() );
								return;
							}
						}
					}
					
					
					//h.state = LockRoom( ship.rooms().random().id );
					h.state = LockDaedalus;
					
					return;
				}
			
			case LockHuman(hr):
				
				var hero = ship.searchTpl( hr, true);
				
				if ( hero != null && !hero.isDead())
				{
					if(Dice.percent( hd.hit + h.currentBonus))
					{
						hero.hurt( h.dmg() );
						h.currentBonus = 0;
					}
					else h.currentBonus += 10;
				}
				else
				{
					h.state = Move;
				}
				
			
			case LockEquipment(_):{
				h.state = Move;
				return;
			}
			
			case LockRoom(_): {
				h.state = Move;
				return;
			}
			
			case LockDaedalus:
			{
				if ( Dice.percent( hd.hit + h.currentBonus))
				{
					var v = ApplyShield( ship, h.dmg() );
					if ( v > 0 )
					{
						ShipLogic.hurtHull( ship, v);
						h.state = Move;
					}
					h.currentBonus = 0;
				}
				else h.currentBonus += 10;
			}
			
			case LockPatrol( r ):
			{
				//dodge percent
				var v = Utils.stanceData( null ).evade;
				
				for(h in listHeroesInRoom( ship,r,false ) )
				{
					var d = Utils.stanceData( h.data.patrolStance );
					v = MathEx.maxi( v, d.evade );
				}
							
				if ( Dice.percent( hd.hit + h.currentBonus - v  ))
				{
					var patrolShip : Patrol = ship.cache.patrols.get( r );
					patrolShip.hurt( h.dmg() );
					h.state = Move;
					h.currentBonus = 0;
				}
				else
				{
					h.currentBonus += 10;
				}
			}
			
			case LockTransport(hid) | LockHunter( hid ):
			{
				var hh = Hunter.manager.get( hid );
				if ( hh == null)
				{
					h.state = Move;
					h.currentBonus = 0;
				}
				else
				{
					if ( Dice.percent( hd.hit + h.currentBonus ))
					{
						hh.hurt( h.dmg(),null );

						h.currentBonus = 0;
						h.state = Move;
					}
					else h.currentBonus += 10;
				}
			}
		}
	
		
	}
	
	
	/**
	 * @return the number of ship emitted
	 */
	public static function unpoolHuntNew( ship : Ship, inqty :Int) : Int {
		var qty = inqty;
		var pool = [HUNTER];
				
		if ( ship.isHardMode() || ship.isVeryHardMode() ) {	
			pool = pool.concat([ASTEROID, TRAX, SPIDER]);
		}
		
		if ( ship.isVeryHardMode())							pool = pool.concat([DICE]);
		//
		
		var poolData : Array<ShipStatsData>= pool.mapa( function( s ) return Protocol.shipStatsDb( s ));
		
		//create the pool
		var poolMax = null;
		var poolMin = null;
		var i = 0;
		for ( p in poolData )
		{
			if ( poolMax == null || (p.aggro_pool_cost > poolData[poolMax].aggro_pool_cost ))
				poolMax = i;
				
			if ( poolMin == null || (p.aggro_pool_cost < poolData[poolMin].aggro_pool_cost ))
				poolMin = i;
				
			i++;
		}
				
		var maxCost = poolData[poolMax].aggro_pool_cost;
		
		//rand is relative to pool cost
		var rd :Array<{id:ShipStatsId,weight:Int,nb:Int}> = 
		{
			poolData.mapa( function(d) return { id:d.id, weight:d.weight,  nb: d.max_nb == null ? 10000 : d.max_nb } );
		}
		
		var pushed = [];
		while ( qty > poolMin ) {
			
			var idx = rd.normalizedRandom();
			var curKind = rd[idx].id;
			
			var data = Protocol.shipStatsDb(curKind);
			var hunterCost = data.aggro_pool_cost;
			
			var h = Hunter.manager.select( $pooled && $ship == ship && $type == curKind.index(), false);
			if( h==null )
			{
				h = new Hunter(ship, data);
				h.insert();
			}
			else h.lock();
			
			//otherwise get previous
			h.state = Move;
			h.pooled = false;
			h.update();
			
			pushed.pushBack( h );
			
			qty -= hunterCost;
			ship.aggroPool -= hunterCost;
			
			rd[idx].nb--;
			
			if ( rd[idx].nb <= 0)
				rd.splice( idx, 1 );
		}
		
		if ( pushed.length > 0) ShipLog.createNeronLog(HUNTER_WAVE_INC, ship).insert();
		
		ship.dirty = true;
		
		return pushed.length;
	}
	
	public static function unpoolHunt( ship : Ship, inqty : Int )
	{
		var qty = inqty;
		
		var tactician = ship.liveHeroes(false).find( function(h) return h.skillsHas(SPACE_TACTICS));
		var tacticApplied = false;
		if( Dice.percent(50) && tactician!=null )
		{		
			qty = Std.int( qty * 0.77 );
			tacticApplied = true;
		}
		
		var nb = unpoolHuntNew(ship, qty);
		
		if( qty>0 && tactician!=null && tacticApplied )
			ShipLog.createPersoLog(  EV_TACTICS_SUCCESS, tactician, SL_Skill(SPACE_TACTICS)).insert();
			
		return nb;
	}
	
	public function onNewHuntCycle()
	{
		if (!Const.ENABLE_HUNTERS) return;
		
		Tools.profBegin("FSM cache");
		var hunterCache  = makeCache(ship);
		Tools.profBegin("FSM cache");
		
		Tools.profBegin("FSM retrieve");
		var hunters : List<Hunter> = Hunter.manager.getActives( ship,true );
		Tools.profBegin("FSM retrieve");
		
		var l = hunters.length;
		var lit = "FSM calc " + l;
		
		Tools.profBegin(lit);
			for (h in hunters)	onHunterNewCycle( h, hunterCache );
		Tools.profEnd(lit);
		
		Tools.profBegin("FSM updt");
			for ( h in hunters) h.update();
		Tools.profEnd("FSM updt");
		
		var d = Std.int(ship.getCurrentAge() / Const.CYCLE_PER_DAY);
		
		var cstAdd = 2;
		
		var poolAdd = 5 + d + cstAdd;
		if ( ship.isHardMode() ) poolAdd++;
		if ( ship.isVeryHardMode() ) poolAdd+=2;
		
		ship.aggroPool += Std.int( poolAdd * ship.getOverloadFactor() + 0.5 );
		
		var unleash : Bool = Dice.percent( 20 );
		var nb = 0;
		
		if ( ship.getCurrentAge() <= 2 ) 
			unleash = false;
		
		if ( !ship.isSafeSycle() && unleash )
		{
			nb=unpoolHunt(ship, ship.aggroPool);
			ship.recache();
		}
		else {
			var d = 10 - ship.getCurrentDay();
			if ( d <= 0 ) d = 0; 
			
			if ( d>=1
			&&	Dice.percent( 5 + d )) {
				var curKind = TRANSPORT;
				var data = Protocol.shipStatsDb(curKind);
				var h = new Hunter(ship, data);
				h.insert();
				
				h.state = Move;
				h.pooled = false;
				h.update();
				
				if( !h.deleted )
					ShipLog.createNeronLog( NERON_TRADE_ARRIVED, ship ).insert();
			}
		}
	}
	
	
	//delabrement
	
	public static function onNewDecayCycle( ship : Ship )
	{
		if ( !ship.isGameStarted()) return;
		
		var lh = ship.liveHeroes();
		
		var age = ship.getCurrentAge();
		var ageDay = Std.int(age / Const.CYCLE_PER_DAY);
		
		var d = 0;
		switch(ageDay)
		{
			case 0: d += 1;
			case 1: d += 2;
			
			default:
			var dayHard = ageDay - Ship.hardModeStart;
			var dayVeryHard = ageDay - Ship.veryHardModeStart;
			
			d += 1;
			
			if ( ship.isVeryHardMode() )
			{
				d += 3 + dayHard + dayVeryHard;
			}
			else if( ship.isHardMode() )
				d += 1 + dayHard;
		}
		
		d = Std.int(d * ship.getOverloadFactor() + 0.5);

		ship.decayPool += d;
		
		if ( Dice.percent(15) && ship.isProjectUnlocked( BRIC_BROC ) ) {
			ShipLog.createNeronLog(EV_DECAY_DELAYED,ship).insert();
			return;
		}
		
			
		
		Debug.MSG("pool val:"+ship.decayPool);
		var rooms = ship.roomData.filter( function(r) return !r.status.has(DETACHED) && !r.status.has(INDESTRUCTIBLE)  );
		var eventList = Protocol.decayEventList.list();
		
		//create initial target set
		var roomFireList = rooms.filter( function(x) return Protocol.roomDb( x.id ).type != PATROL_SHIP );
		var roomDoorList = rooms.filter( function(x) return Protocol.roomDb( x.id ).type != PATROL_SHIP );
		
		var objMap = ship.objectMap();
		
		var equipList = objMap.filter( function(om) return om.item.id != DOOR
															&& !om.item.status.has(BROKEN)
															&& om.item.status.has(EQUIPMENT)
															&& om.item.id != OXYGEN_TANK
															&& om.item.id != FUEL_TANK
															&& om.item.canBreak()
															);
															
		var oxyList = 	objMap.filter( function(om) return om.item.id == OXYGEN_TANK 	&& !om.item.status.has(BROKEN) );
		var fuelList = 	objMap.filter( function(om) return om.item.id == FUEL_TANK 		&& !om.item.status.has(BROKEN) );
		var joltedList = rooms.filter( function(r) return ShipLogic.listHeroesInRoom( ship, r.id , true ).length > 0 );
		
		var decayHeroes = function() return ship.liveHeroes(true).filter( function(h)return h.expe == null );
		var anxietyList = decayHeroes().filter( function(h) return !h.isMush());
		var disList = decayHeroes().filter( function(h) return !h.isMush());
		
		var accidentList = decayHeroes();
		var electrocutionList = rooms.filter( function(x) return Protocol.roomDb( x.id ).type != PATROL_SHIP );
		
		var renewList = function()
		{
			eventList = eventList.filter(
				function(e)
				{
					switch(e.id)
					{
						case A_FIRE:
							roomFireList = roomFireList.filter( function(x) return  !x.status.has(FIRE) );
							if ( roomFireList.length <= 0 ) return false;
								
						case DOOR_BLOCKED:
							roomDoorList = roomDoorList.filter( function(x) return x.inventory.hasWorking( DOOR ));
							if( roomDoorList.length <= 0) return false;
							
						case MAIN_EQUIPMENT_FAILURE:	if ( equipList.length <= 0) return false;
						case O2_LEAK:					if ( oxyList.length <= 0) return false;
						case FUEL_LEAK:					if ( fuelList.length <= 0) return false;
						case JOLT: 						if ( joltedList.length <= 0) return false;
								
						case BOARD_DISEASE: 			if ( disList.length <= 0) return false;
						case ANXIETY_ATTACK:			if ( anxietyList.length <= 0) return false;
						
						case ELECTROCUTION: 			if ( electrocutionList.length <= 0) return false;
						case ACCIDENT:					if ( accidentList.length <= 0) return false;
					}
					
					return e.decayPoint < ship.decayPool;
				});
		};
		
		//add ods
		var shallLaunch = RandomEx.randF() <= (ship.decayPool / Gameplay.DECAY_POINTS_BASE );
		if ( !shallLaunch ) return;
		
		var upper = 2000;
		
		renewList();
		while( eventList.length > 0)
		{
			upper--;
			if ( upper <= 0)
			{
				App.current.logError("problem doing decay");
				break;
			}
			
			var event = RandomEx.normRdEnum( eventList );
			var evtData = Protocol.decayEventList[event.index()];
			ship.decayPool -=  evtData.decayPoint;
			
			//Debug.MSG("decay go : pool = " + ship.decayPool);
			//Debug.MSG("decay event: eval :" + event);
			
			switch(evtData.id)
			{
				case A_FIRE:
					var rm =  roomFireList.random();
					roomFireList.remove( rm );
					ShipLogic.setFire(ship, rm.id );
					
				case O2_LEAK:
				{
					var o2 = oxyList.random();
					if( o2 != null)
					{
						oxyList.remove( o2 );
						ShipLog.createNeronLog(DECAY_O2,ship).insert();
						dmgObject2( ship, {item:o2.item,rid: o2.rid, inv:o2.inv });
						ship.o2 = MathEx.clampi( ship.o2, Const.MIN_OXY, Const.MAX_OXY );
					}
				}
				
				case ELECTROCUTION:
				{
					var r = electrocutionList.random();
					electrocutionList.remove( r);
					
					ShipLog.createRoomLog( EV_ELECTROCUTION,ship, r.id ).insert();
					for ( h in ShipLogic.listHeroesInRoom( ship, r.id , true ) )
						h.hurt( 3, DC_ELECTROCUTED );
					
					for ( e in r.inventory.list())
						if ( e.status.has(EQUIPMENT) && e.id != DOOR && !e.status.has(BROKEN))
							dmgObject2( ship, {item:e, rid:r.id, inv:r.inventory} );
				}
				
				case ACCIDENT:
				{
					var h = accidentList.random();
					accidentList.remove( h);
					
					ShipLog.createHeroLog( EV_ACCIDENT, h ).insert();
					h.hurt( Dice.roll(4, 6), DC_DAEDALUS_ACCIDENT );
				}
				
				case FUEL_LEAK:
				{
					var fuel = fuelList.random();
					if ( fuel != null)
					{
						fuelList.remove(fuel);
						ShipLog.createNeronLog(DECAY_FUEL,ship).insert();
						dmgObject2( ship, fuel);
					}
				}
				
				case JOLT:
				{
					var room = joltedList.random();
					joltedList.remove( room );
					
					var hr = ShipLogic.listHeroesInRoom( ship, room.id, true );
					for(h in hr)	h.hurt(Dice.roll( 1, 3 ));
					
					ShipLog.createShipLog( ship.ruleFlags.has(Rule.GRAVITY_SIMULATOR) ? DECAY_JOLT_SIM : DECAY_JOLT_NO_SIM, ship).setRid(room.id).insert();
				}
				
				case MAIN_EQUIPMENT_FAILURE:
				{
					var equip = equipList.random();
					if ( equip != null)
					{
						equipList.remove(equip);
						ShipLog.createNeronLog(NERON_EQUIPMENT_FAILURE, ship, equip.item.pack() ).insert();
						dmgObject2( ship, equip );
					}
				}
				
				case DOOR_BLOCKED :
				{
					var cand = roomDoorList.random();
					for(d in cand.inventory)
					{
						if(d.id == DOOR)
						{
							dmgObject2( ship, {item:d,rid:cand.id,inv:cand.inventory} );
							break;
						}
					}
				}
				
				case ANXIETY_ATTACK:
				{
					var rd = anxietyList.random();
					if ( rd != null)
					{
						anxietyList.remove( rd );
						ShipLog.createHeroLog( LOG_ANXIETY_ATTACK, rd ).set(Personnal).insert();
						rd.decrMoral( 3 );
					}
				}
				
				case BOARD_DISEASE:
				{
					var dl = Protocol.diseaseList.filter( function( d) return d.gain_on.has( Board ) );
					var hero = disList.random();
					
					disList.remove( hero );
					var t = 50;
					ActionLogic.iterEffects( hero, function(fx)
						switch(fx)
						{
							case IncrDisease: t += 10;
							default:
						}
					);
					
					if( hero.isMoralGood() )
						t -= 30;
					
					if ( Dice.percent( t ) )
						hero.disease( RandomEx.normRdEnum(dl) );
				}
			}
	
			Debug.ASSERT( ship.decayPool >=  0);
			renewList();
		}
		
		ship.update();
	}
	
	
	
	public static function onNewFireCycle(ship : Ship )
	{
		var neron = 0;
		var auto = ship.isProjectUnlocked(AUTO_WATERING);

		var allFirerooms = ship.rooms().filter(function(r) return r.status.has(FIRE));
		
		var isFireable = function(r:RoomInfos) return
		!r.status.has(FIRE)
		&& !r.status.has(INDESTRUCTIBLE)
		&& Protocol.roomDb( r.id).type != PATROL_SHIP;
		
		var nbSpread = MathEx.mini( Gameplay.FIRE_MAX_SPREADS, allFirerooms.length );
		
		for(x in allFirerooms)
		{
			if( auto && Dice.percent( Gameplay.FIRE_AUTOWATERING_PC))
			{
				x.status.unset( FIRE );
				neron++;
				continue;
			}
			
			for(h in ShipLogic.listHeroesInRoom( ship, x.id, true ) )
				h.hurt( Dice.roll( Gameplay.FIRE_HERO_DMG_MIN, Gameplay.FIRE_HERO_DMG_MAX), DC_BURNT );

			if( Dice.percent(  Gameplay.FIRE_HULL_DMG_PC ) )
				hurtHull( ship, Dice.roll( Gameplay.FIRE_HULL_DMG_MIN, Gameplay.FIRE_HULL_DMG_MAX) );
			
			for( it in x.inventory.list() )
				if ( it.id != DOOR && Dice.percent(  Gameplay.FIRE_OBJ_DMG_PC) )
				{
					if ( it.id == HELP_DRONE  )
						if ( it.getDroneInfos().pawa.has( DPU_EXTINGUISH_FIRE))
							continue;
							
					dmgObject(ship, {item:it,rid:x.id,inv:x.inventory}, x.id, true);
				}
		}
		
		for (r in allFirerooms.scramble() )
		{
			if ( Dice.percent(Gameplay.FIRE_SPREAD_PC))
			{
				var adj = calcPhysAdjRooms( ship, r.id, isFireable).random();
				if ( adj != null)
				{
					new AdminStream(ship,"spreading fire from "+r.id+" to "+adj.id);
					setFire( ship, adj.id);
					nbSpread--;
				}
			}
			if ( nbSpread == 0) break;
		}
		
		if ( neron > 1 )		ShipLog.createNeronLog(NERON_EXTINGUISH,		 	ship, Qty(neron) ).insert();
		else if ( neron == 1)	ShipLog.createNeronLog(NERON_EXTINGUISH_SINGLE, 	ship).insert();
	}
	
	public static function getNbOxyLeak( ship :Ship )
	{
		return ship.roomData.fold( function( r ,res ) return r.inventory.count( function(x) return x.id == OXYGEN_TANK && x.status.has(BROKEN)) + res
									,0);
	}

	public static function updatePot( ship:db.Ship, item : ItemLocation )
	{
		var it = item.item;
		var rm = item.rid;
		var rmd = ship.getRoom( rm );
		var skInfos = it.getSkin();
		var charge = it.getChargeParam();
		
		if (skInfos == null)
			return;
		
		var skinData : TreeData = ship.data.trees[ skInfos.v ];
		var isGarden = Protocol.roomDb( item.rid ).type == HYDROPONIC_CENTER;
		var mkOxy = !it.status.has( PLANT_DISEASED ) && ! it.status.has(PLANT_DRY);
		var mkFruit = !it.status.has( PLANT_DISEASED ) && ! it.status.has(PLANT_DRY) && !it.status.has(PLANT_THIRSTY);
		var isYoungling = false;
		
		var log = function(e) ShipLog.createRoomLog( e, ship, rm,it.pack() ).insert();
		
		function clearYoungling() {
			it.removeCharges();
			it.status.unset(PLANT_YOUNGLING);
			log( PLANT_MATURED );
			isYoungling = false;
		}
		
		if ( charge != null )
		{
			it.incrChargeParam();
			if ( isGarden && ship.isProjectUnlocked( HYDROPONIC_INCUBATOR ))
				it.incrChargeParam();
			charge = it.getChargeParam();
			
			var g = it.getChargeParamM();
			
			if ( isGarden && ship.isProjectUnlocked(PARASITE_ELIM) )
				g -= 4;
				
			if ( charge >= g)
			{
				clearYoungling();
			}
			else
			{
				it.status.set(PLANT_YOUNGLING);
				isYoungling = true;
			}
		}
		else {
			if( it.status.has( PLANT_YOUNGLING ))
				clearYoungling();
		}
		
		var skinText : SkinData = Protocol.skinList[ skInfos.v ];
		var nb = 1;
		if ( isGarden && ship.isProjectUnlocked( HEAT_LAMP ) && Dice.percent(50) )
			nb++;
		
		if (ship.isMidnight() && !isYoungling)
		{
			var skinRes = Skin( SK_FRUIT, skInfos.v );
			
			if (mkFruit) {
				function spawnFruit(skRes) {
					var desc = Utils.itemDesc( ship, CONSUMABLE );
					desc.customInfos.push( skRes );
					
					if ( skInfos.v >= Protocol.skinList.length )
						throw "assert";
						
					ShipLog.createRoomLog( FRUIT_MATURED, ship, rm,ItemSkin( desc.id, SK_FRUIT, skInfos.v )).insert();
					
					if( isGarden && ship.isProjectUnlocked(FOOD_RETAILER) ) {
						var rf = ship.getRoom( RoomId.REFECTORY );
						
						ShipLog.createRoomLog( OBJECT_TRANSPORTED, ship, rm, ItemSkin( desc.id, SK_FRUIT ,skInfos.v ) ).insert();
						ShipLog.createRoomLog( OBJECT_TRANSPORTED, ship, rf.id, ItemSkin( desc.id, SK_FRUIT ,skInfos.v ) ).insert();
						
						rf.inventory.push( desc );
					}
					else
						rmd.inventory.push( desc );
				}
				
				for ( i in 0...nb)
					spawnFruit(skinRes);
			
				if ( ship.isHalloween())
				{
					var d = 5;
					if ( skInfos.v > 0) d <<= 2;
					if( Dice.percent2(d)){
						skinRes = Skin( SK_FRUIT, 13 );
						spawnFruit(skinRes);
					}
				}
			}
	
			if ( mkOxy )
				ship.o2 = MathEx.clampi( ship.o2 + 1, Const.MIN_OXY, Const.MAX_OXY );
		}
		
		if (ship.isMidnight() )
		{
			if (it.status.has( PLANT_THIRSTY ) )
			{
				it.status.unset( PLANT_THIRSTY  );
				it.status.set( PLANT_DRY );
			}
			else if ( it.status.has( PLANT_DRY ) ) // die
			{
				log( PLANT_DICEASED );
				it.status.unset( PLANT_DRY );
				it.status.unset( PLANT_DISEASED );
				it.status.unset( PLANT_THIRSTY );
				it.status.unset( PLANT_YOUNGLING );
				it.removeCustomInfos( Skin(null, null));
				it.removeCustomInfos( Charges(null, null));
				
			}
			else
			{
				it.status.set( PLANT_THIRSTY );
			}
		}
			
		if (Dice.percent(3))
			it.status.set(PLANT_DISEASED);
			
		ship.dirty = true;
	}
	
	
	public static function neronSelfCompute(ship:Ship)
	{
		if (  ship.isProjectUnlocked(NERON_PROJECT_THREAD) )
		{
			for(p in ship.projectData.filter( function(pr) return pr.progress > 0 && pr.active == true && !pr.deleted))
			{
				p.progress = MathEx.clampi( p.progress + 5, 0, 100);
				if(p.progress>=100)
				{
					ActionLogic.validateProject(ship, p,true);
					break;
				}
			}
		}
	}
	
	public static function getContamList( len : Int) 
	{
		var r = new List();
		var n = 0;
		var p = Dice.roll( 0 , 3 );
		
		for( i in 0...len )
		{
			var op = p;
			p = Std.int(p + (Dice.roll( 2, 6)) );
				
			r.pushBack( p );
			n++;
		}
		
		return r;
	}
	
	public function onNewCycle( )
	{
		Tools.profBegin( "onNewCycle" );
		
		if( ship.destroyed )
			return;
		
		var c = ship.currentCycle;
		var sl = new ShipLogic(ship);
		//LOCK EVERYONE TO PREVENT DEADLOCKgetPendingShip
		var users : List<User> = User.manager.lockUsers(ship);
		var msg = "Maintained cycle : " + ship.currentCycle;
		Debug.MSG(msg);
		new AdminStream( ship, msg );
		Debug.ASSERT(ship != null);
		
		if( ship.data.lastTravel != null )
			if ( ship.data.lastTravel.date +  ship.data.lastTravel.duration == c )
			{
				var pl = ship.getPlanet();
				ShipLog.createNeronLog( DAEDALUS_ARRIVED , ship ).insert();
				
				if ( pl != null)
				{
					var frieda = ship.searchLiveTpl( FRIEDA_BERGMANN, true);
					if ( frieda != null )
						frieda.triumph( PLANET_FINDER );
				}
				
				var nb = ship.aggroPool >> 1;
				if ( ship.isProjectUnlocked( TRAIL_REDUCER))
					nb >>= 1;
				unpoolHunt( ship, nb);
				ship.recache();
			}
		
		var heroes : List<Hero> = ship.liveHeroes( true );
		
		var onMidnight = (ship.currentCycle % Const.CYCLE_PER_DAY) == 0;
		var onEmbarkTime = (ship.currentCycle % Const.CYCLE_PER_DAY) == Const.EXPEDITION_DEPARTURE_CYCLE;
		
		var n = Utils.now( ship.currentCycle);
		ShipLog.timeChange(ship, n.day, n.cycle);
		
		var planet : DbPlanet = ship.getPlanet();
		
		//neron ops
		{
			var l = heroes.filter( function(h) return h.isMush());
		
			//all crew is mush
			if(!ship.isFungalBattle())
				if ( l.length == heroes.length && l.length > 0)
				{
					for ( h in l)
						h.triumph( MUSH_VICTORY );
					
					ShipLog.createNeronLog( NERON_ALL_CREW_MUSH, ship ).insert();
					new ShipLogic(ship).destroyShip( DC_NERON_DESTROYED_DAEDALUS );
					return;
				}
			
			neronSelfCompute(ship);
			
			//kill comm
			if( Dice.percent(3) )
				ShipLogic.killLink( ship );
			
			//update rebels
			for( rb in  ship.data.com.rebelData )
			{
				if ( rb.decoded ) continue;
				if ( rb.lost ) continue;
				
				if ( ship.currentCycle > rb.signalEnd )
					rb.lost = true;
					
				if ( ship.currentCycle == rb.signalStart && ship.data.com.isLinked )
					ShipLog.createNeronLog( REBEL_SIGNAL_DETECTED, ship ).insert();
			}
			
			for ( d in ship.data.fkDelayer )
				d.dc--;
			
			ship.data.fkDelayer = ship.data.fkDelayer.filterA( function(d) return d.dc == 0);
		}
		
		for ( r in ship.roomData)
			r.status.unset( DELOGED );
				
		//update in case of deaths
		heroes = ship.liveHeroes(true);
		
		//oxy consumption
		var base = 1;
		if ( ship.isProjectUnlocked( OXY_MORE ) && Dice.percent( 20 ) ){
			base = 0;
			ShipLog.createNeronLog(EV_OXY_MORE, ship).insert();
		}
		
		var o2Decr =  base + getNbOxyLeak(ship);
		ship.o2 = MathEx.clampi( ship.o2 - o2Decr, Const.MIN_OXY, Const.MAX_OXY );
		if( ship.o2 <= 0 )
		{
			var dispPerOxy = heroes.dispatch( function(h)
				return h.inventory.count(function(o) return o.id == OXY_CAPSULE)
			);
			
			for( i in 0...Const.MAX_HERO_INV_SIZE+1 )
			{
				var l  = dispPerOxy.get( i );
				if ( l == null || l.length == 0) continue;

				var h = l.random();
				var o = h.inventory.findWorking( OXY_CAPSULE );
				
				if ( o == null)
				{
					//ShipLog.createNeronLog( OXY_LOW_DAMMIT, ship).insert();
					killMember( h, DC_OXYGEN );
				
					heroes = heroes.filter( function(h) return !h.isDead() );
				}
				else
				{
					ShipLog.createPersoLog( OXY_LOW_YOU_MISSED_DEATH, h).insert();
					h.inventory.removeUid( o );
				}
				break;
			}
		}
		
		//maintain cat and autonomies
		for(her in heroes)
		{
			var hloc = her.loc();
			if ( 	hloc.getTypeData().id == RoomType.PATROL_SHIP
			&&		hloc.status.has(DETACHED))
			{
				var patrol = her.getPatrol( true);
				patrol.tick();
			}
			
			if( !her.flags.has( SLEEPY )
			&&	her.currentPa[ GEN.index()] == her.maxPa( GEN )
			&&	her.lastSeen.addHours(24).getTime() < Date.now().getTime() )
			{
				her.flags.set( SLEEPY );
				her.pFlags.set( HAS_SLEEPED_ONCE );
				ShipLog.createHeroLog( FALLS_ASLEEP, her).insert();
				var st = her.user.stats(true);
				
				st.sleeps++;
				
				st.update();
			}
			
			if ( her.isShelled()) continue;
	
			//check space suit autonomy
			if( hloc.getTypeData().id == OUTER_SPACE )
			{
				var hasOxy = false;
				for (it in her.inventory)
				{
					if ( it.id != SPACE_SUIT ) continue;
					if ( it.status.has( BROKEN ) ) continue;
					
					it.customInfos = it.customInfos.fold( function(x : ItemInfos,r  :List<ItemInfos>)
					return switch(x)
					{
						case Autonomy( d ):
						if (d > 0)
						{
							hasOxy = true;
							ShipLog.createHeroLog( AUTONOMY, her, Qty( d )).set(Personnal).insert();
							r.push( Autonomy(d - 1) );
							return r;
						}
						else
						{
							return r;
						}
						default:
						r.push(x);
						return r;
					},new List()
					);
				}
				
				if ( !hasOxy)
					killMember(her, DC_OXYGEN_SUIT);
			}
		}

		var objMap = ship.objectMap();
		
		//looped back to midnight
		if ( onMidnight )
		{
			ship.dailyFlags.unset(NERON_ADMIN_CPU_PRIO_DONE_TODAY);
			//reset dayli data
			ship.sporeThisDay = 0;
			ship.data.com.hitList.clear();
			
			objMap.iter(function( p )
			{
				switch(p.item.id)
				{
					default:
					case CONSUMABLE:
					{
						var sk = p.item.getSkin();
						if( sk == null ) return;
						
						var item = p.item;
						switch(sk.t)
						{
							default://skip
							case SK_RATION,SK_FRUIT:
							if( sk.t == SK_RATION )
								if( Protocol.rations[sk.v].effect.has( PACKAGED ) )
									return;
								
							if( ship.neronVal( NB_ALLOW_PERISHMENT ) == 0 )
							{
								ShipLog.createLocalNeronLog( EventId.RATION_PERISHED, ship ,p.rid).insert();
								p.inv.removeUid( p.item );
							}
							else
								corruptItem( ship, p.item );
						}
					}
				}
			}
			);
		}
		
		var drones = [];
		
		objMap.iter(function( p )
		{
			if(	p.item.status.has(WORLD))
			{
				var rl = Protocol.worldItem2Rules(p.item.id);
				if (rl != null)
					if ( p.item.status.has(BROKEN))
					{
						if( ship.ruleFlags.has( rl ) )
						{
							ship.ruleFlags.unset( rl);
							switch(p.item.id)
							{
								case GRAVITY_SIMULATOR: ShipLog.createNeronLog( GRAV_SIM_OFF, ship ).insert();
								default:
							}
						}
					}
					else
					{
						if( !ship.ruleFlags.has( rl ) )
						{
							ship.ruleFlags.set( rl );
							switch(p.item.id)
							{
								case GRAVITY_SIMULATOR: ShipLog.createNeronLog( GRAV_SIM_ON, ship ).insert();
								default:
							}
						}
					}
			}
			
			if( !p.item.status.has(BROKEN) )
			switch(p.item.id)
			{
				default:
					if( !p.item.status.has( NO_CHARGE ))
						p.item.incrChargeParam();
					
				case TURRET_COMMAND:
					p.item.incrChargeParam();
					if(  ship.isProjectUnlocked(TURRET_EXTRA_FIRERATE ) )
						p.item.incrChargeParam();
				
				case PATROL_COMMAND, PASIPHAE_COMMAND:
					if( !ship.getRoom( p.rid ).status.has(DETACHED) )
						p.item.incrChargeParam();
						
				case GRAVITY_SIMULATOR:
					
				case TREE_POT:
					updatePot( ship,p );
					
				case HELP_DRONE:
					drones.pushBack( p );
					
				case BEAT_BOX:
					if ( ship.isProjectUnlocked( BEAT_BOX )){
						p.item.removeCustomInfos( Song( null ) );
						
						var sidx = Std.random(Protocol.songs.length );
						if ( sidx != null)
						{
							var sgData = Protocol.songs[sidx];
							p.item.customInfos.push( Song( sidx ) );
							ShipLog.createRoomLog( BEAT_BOX_NEW_CYCLE, ship, p.rid, Data( Text.fmtSong( { tit:sgData.name, by:sgData.author } ) ) )
							.insert();
							for ( h in ShipLogic.listHeroesInRoom( ship, Gameplay.BB_BOX_ROOM, true ))
								h.testSong();
						}
					}
			}
			
			for ( ci in p.item.customInfos ) 
				switch(ci) {
					default: 
					case DelayedEffect(1, e) | DelayedEffect(0, e):  
						p.item.customInfos.remove(ci);
						switch(e) {
							default:
							case BreakItem:
								ShipLog.createNeronLog(NERON_EQUIPMENT_FAILURE, ship, p.item.pack() ).insert();
								ShipLogic.dmgObject2(ship, p);
						}
						
					case DelayedEffect(d, e):  
						p.item.customInfos.remove(ci);
						p.item.customInfos.pushFront(DelayedEffect(d-1,e));
				}
		});
		

		onNewDroneCycle( drones );
		
		
		//any mush heroes including dead ones
		for ( h in ship.heroes(true) )
			if ( h.isMush() )
				h.triumph( CYCLE_MUSH );

		//update in case of deaths
		heroes = ship.liveHeroes(true);
		
		
		ship.dailyPaConsumption = 0;
		//update heroes
		for (x in heroes )
		{
			ship.dailyPaConsumption += x.consummedPa;
			onNewHeroCycle( x );
			x.consummedPa = 0;
			
			x.update();
		}
		
		heroes = ship.liveHeroes(true);
		
		//do cycle ops per room
		//for(x in ship.roomData)
		//	updateCat(ship, x);
		
		//update side effects
		//DAILY
		for(x in heroes)
		{
			var effx = new List();
			x.data.wounds.iter( function( w )
									if( Protocol.woundDb( w).effects!= null)
										for (fx in Protocol.woundDb( w).effects )
											effx.push(fx)
								);
								
			x.data.diseases.iter( 	function( d )
										if( d!=null && Protocol.diseaseDb( d.id ).effects != null )
										for (fx in Protocol.diseaseDb( d.id ).effects )
											effx.push(fx)
								);
			//periodic effects
			effx.iter(
			function(fx)
			{
				switch(fx)
				{
					case OnCycle( fx ): ActionLogic.dfltEffectHandler( x, fx );
					default:
				}
			});
			
			x.data.sched = x.data.sched.fold( function(e,r : List<{delay:Int,fx:OdsEffect}>)
			{
				if ( e.delay <= 1 )
					ActionLogic.dfltEffectHandler( x, e.fx );
				else r.push( { delay:e.delay - 1, fx:e.fx } );
				
				return r;
			},new List());
		}
		
		var hasPilgred = ship.pilgred.progress >= 100;
		
		if ( hasPilgred && (ship.currentCycle % 8) == 2 )
			ship.data.dailyAcHitList.unset( RETRIEVE_COFFEE );
			
		if ( hasPilgred && (ship.currentCycle % 8) == 6 )
			ship.data.dailyAcHitList.unset( RETRIEVE_COFFEE );
			
		if ( hasPilgred || ((ship.isProjectUnlocked(FISSION_COFFEE_ROASTER)) && ((ship.currentCycle % 8) == 3)))
			ship.data.dailyAcHitList.unset( RETRIEVE_COFFEE );
		
		for (x in heroes )
		{
			if( !x.isDead() && !x.isMush())
			{
				var usr = x.getOwner();
				Debug.NOT_NULL(usr);
			
				if( !x.isAsleep())
					x.triumph( CYCLE_HUMAN );
				else
					x.triumph( CYCLE_HUMAN_SLEEPY );
				
				if ( onMidnight && x.heroId == ZHONG_CHUN )
					x.triumph( CHUN_LIVES );
					
				if ( ship.getCurrentDay() == 5 ) {
					var st = x.user.stats(true);
					st.nbDay5Passed++;
					st.update();
				}
			}
		}
		
		if (  onMidnight )
		{
			ship.data.dailyAcHitList.clear();
			for (x in heroes )
			{
				if( !x.isDead())
				{
					if( Protocol.roomType( Protocol.roomDb(x.location).id ).id == PATROL_SHIP )
					{
						if( !x.loc().inventory.hasWorking( PATROL_COMMAND )
						&&	!x.loc().inventory.hasWorking( PASIPHAE_COMMAND )
						&&	!x.skillsHas(ENGINEER))
						{
							ShipLog.createHeroLog( EJECT_FORCED, x ).setRid(null).insert();
							HeroLogic.eject( x );
						}
					}
				}
				
				x.data.wounds.iter(
				function(w)
				{
					var fs = Protocol.woundDb(w).effects;
					if ( fs != null )
					{
						for ( f in fs)
						switch(f)
						{
							case OnDay( fx ): ActionLogic.dfltEffectHandler( x, fx );
							default:
						}
					}
				});
				
				x.data.dailyInfect = 0;
			}
		}
		
		//shield ops
		if ( 	ship.isProjectUnlocked( PLASMA_SHIELD )
		&& 		ship.neronVal(NB_ENABLE_PLASMA_SHIELD)==0 )
			ship.shield = MathEx.clampi( ship.shield + 5,0,100);
		
		//projects stall
		if( ship.data.freezeProjects >= 0 )
		{
			ship.data.freezeProjects--;
			
			if( ship.data.freezeProjects == 0)
			{
				ship.drawProjects();
				ship.data.freezeProjects = -1;
			}
		}
		
		//update decay and fire
		onNewDecayCycle(ship);
		onNewFireCycle(ship);
		
		//update hunt after to prevent quick fire spread
		Tools.profBegin( "onNewHuntCycle" );
		sl.onNewHuntCycle();
		Tools.profEnd( "onNewHuntCycle" );
		
		remapTitles(ship);
		
		var t =
		{
			activity: sl.getActivityLevel(),
			o2:ship.o2,
			fuel:ship.fuel,
			decay:ship.decayPool,
			hunt:ship.aggroPool,
			pop:ship.liveHeroes(false).length,
		};
		//ShipLog.createShipLog( ADM_STATS, ship, AdminDataView(t) ).set(Admin).insert();
		
		//last modif row
		for (x in heroes )
		{
			x.updateFlags();
			x.update();
		}
		
		ship.update();
		
		Tools.profEnd( "onNewCycle" );
	}
	
	
	public static function onNewHeroCycle( hero : Hero  )
	{
		var ship = hero.ship;
		
		//am i dead?
		if ( hero.flags.has(DEAD) ) return;
		
		for(d in hero.data.diseases)
		{
			
			if(d!=null)
			switch(d.id)
			{
				default:
				case CRABISM:
				{
					if (Dice.percent(4))
					{
						ShipLog.createHeroLog( SYMPTOM_CRABISM1, hero ).insert();
						ActionLogic.dfltEffectHandler( hero, LosePA( GEN, 1));
					}
					else if (Dice.percent(4))
					{
						ShipLog.createHeroLog( SYMPTOM_CRABISM2, hero ).insert();
						ActionLogic.dfltEffectHandler( hero, LosePA( MOV, 2));
					}
					else if (Dice.percent(4))
					{
						ShipLog.createHeroLog( SYMPTOM_CRABISM3, hero ).insert();
						ActionLogic.dfltEffectHandler( hero, DealDmg( 1 ));
					}
				}
				
				case COPROLALIA:
				case PARANOIA:
			}
		
			if ( d!=null && d.tick() )
			{
				hero.data.diseases.remove( d.id );
				ShipLog.createHeroLog( DISEASE_END,hero,Disease(  d.id ) ).set( Personnal ).insert();
			}
		}
		
		if ( hero.flags.has( UNSOCIABLE) )
		{
			if ( ShipLogic.listHeroesInRoom( ship,hero.location , false ).length >= 2)
			{
				ShipLog.createHeroLog( HERO_UNSOCIABLE,hero ).set(Personnal).insert();
				hero.decrMoral( 1 );
			}
		}
		
		if ( hero.flags.has( GERMAPHOBIC ) && hero.flags.has(DIRTY) )
		{
			ShipLog.createPersoLog( EV_GERMAPHOBIC,hero ).set(Personnal).insert();
			hero.decrMoral( 1 );
		}
		
		if ( hero.flags.has( LOST_ON_PLANET ) )
		{
			ShipLog.createPersoLog( HERO_LOST_DEPRESSED,hero ).insert();
			hero.decrMoral( 2 );
		}
		
		if ( Dice.percent(Const.CONTACT_ODD) )
			HeroLogic.onContact(hero);
			
		var onMidnight : Bool = (hero.ship.currentCycle % Const.CYCLE_PER_DAY) == 0;
		
		//get some pa
		for(c in PAColors.array())
		{
			var pac = hero.paPerCycle(c);
			hero.addPa(c, pac);
			
			if ( onMidnight )
			{
				var pad = hero.paPerDay(c);
				hero.addPa(c, pad);
			}
		}
		
		if ( ship.isInPanic() && hero.skillsHas(PANIC))
		{
			ShipLog.createPersoLog( SKILL_ADD_PA, hero, SL_Skill( PANIC )).insert();
			hero.addPa( GEN, 1);
			hero.addPa( MOV, 1);
		}
		
		//every midnight
		if( onMidnight )
		{
			var v = 0;
			
			if(!hero.skillsHas(OPTIMISTIC))
				v++;
			
			var hasHope = ship.liveHeroes( false ).test(function(h) return h.skillsHas(MANKIND_ONLY_HOPE ) );
			if(!hasHope)
				v++;
			
			if ( v > 0 && !hero.isMush() )
			{
				hero.decrMoral( v );
				ShipLog.createPersoLog( ONE_MORE_DAY, hero, Qty( v ) ).insert();
			}
			
			hero.sporeThisDay=0;
			hero.data.dailyPick.clear();
			
			if ( hero.hp < hero.getMaxHp() )
				hero.grantHp( 1 );
		}
		
		if ( hero.skillsHas(LOGISTICS))
		{
			var h =  listHeroesInRoom( ship, hero.location , true )
			.filter(function(h) return h != hero )
			.random();
			
			if( h!=null )
			{
				ShipLog.createPersoLog( SKILL_ADD_PA_LOGISTICS, h, Subject( hero.heroId ) ).insert();
				h.addPa( GEN, 1);
			}
		}
		
		//be hungry
		if(!hero.hasSymptoms(NAUSEA))
			hero.nurture--;
		
		if ( Const.DIE_BY_STARVATION && !hero.isMush() && hero.flags.has(STARVING ))
		{
			ShipLog.createHeroLog( EXTREME_HUNGER ,hero ).set(Personnal).insert();
			hero.nurture = Const.DEATH_NURTURE;
					
			hero.hurt( 1, DC_STARVING );
		}
		
		if( Const.DIE_BY_MORAL )
			if ( hero.moral <= 0 )
				if ( hero.skillsHas(ABNEGATION)){
					hero.hurt(1, DC_SUICIDE);
					ShipLog.createPersoLog( EV_ABNEGATION, hero ).insert();
				}
				else 
					killMember( hero , DC_SUICIDE );
		
		if ( hero.moral < Const.MAX_MORAL / 4)
			if ( Dice.percent(15) )
				hero.disease( DEPRESSION );
	
		if ( onMidnight)
		{
			var d = ship.getCurrentDay()+1;//allready tomorrow you now

			if ( d == 4 )
				hero.goal( com.Goal.id.day_5_reached );
		
			if ( d == 9 )
				hero.goal( com.Goal.id.day_10_reached );
				
			if ( d == 14 )
				hero.goal( com.Goal.id.day_15_reached );
				
			if ( d == 19 )
				hero.goal( com.Goal.id.day_20_reached );
				
			if ( d == 29 )
				hero.goal( com.Goal.id.day_30_reached );
			
			hero.maxedGoal( com.Goal.id.day_max, d + 1 );
		}
		
		hero.tick();
		hero.user.onCycle();
	}
	
	public static function cleanupScan( hero : Hero )
	{
		hero.data.currentScan = [];
	}

	public static function hasProjectBoost( ship : Ship , item : ItemId, pr : ProjectId)
	{
		if ( !ship.worldItem2Rooms.exists( item ) ) return false;
		
		var ri = ship.worldItem2Rooms.get( item );
		var it = ri.inventory.findWorking( item );
		if ( it == null) return false;
		
		return it.testProjectPower( pr );
	}
	
	public static function firstHeroInRoom(ship:Ship,r:RoomId,lock) : Hero
	{
		return ship.liveHeroes( lock ).find( function(h) return ( r == h.location)
		);
	}
	
	/**
	 * returns only live heroes
	 */
	public static function listHeroesInRoom(ship:Ship,r:RoomId,lock) : List<Hero>
		return ship.liveHeroes( lock ).filter( function(h) return ( r == h.location) );
	
	/**
	 * returns only live heroes
	 */
	public static function listHeroesInRoomType(ship:Ship,rt:RoomType,lock) : List<Hero>
		return ship.liveHeroes( lock ).filter( function(h) return ( h.locType() == rt) );
	
	public inline static function lsPj( ship, r, lock = false)
		return  listHeroesInRoom(ship, r, lock);
		
	public inline static function lsPnj( ship, r, lock = false)
		return  listPNJInRoom(ship, r, lock);
	
	public static function listPNJInRoom(ship:Ship,r:RoomId,lock) : List<db.PNJ>
	{
		return db.PNJ.manager.search( $ship == ship, lock).filter( function(h)
			return ( r == h.location && !h.dead )
		);
	}
	
	
	public static function tryJump(h : Hero, leaveOrbit : Bool ) : Void {
		var s = h.ship;
		
		if ( s.cache.hunters.test( function(ht) return ht.dead == false && ht.type == SPIDER.index()))
		{
			ShipLog.createHeroLog( SHIP_SPIDER_TRAILING, h ).insert();
			return;
		}
		
		if ( s.enginePipeline <= 0)
		{
			ShipLog.createHeroLog( SHIP_NO_FUEL, h ).insert();
			return;
		}
		
		//retrieve planet
		var hlist = s.heroes(true);
		var curPlanetList : List<DbPlanet> = new List();
		
		for( l in hlist )
			for ( cs in l.data.currentScan)
				if (cs != null)
					curPlanetList.add( cs );
		
		//calc propulsion capabilities
		var hasReactor = s.getRoom( RoomId.MOTOR_ROOM ).inventory.hasWorking( EMERGENCY_REACTOR );
					
		if ( !hasReactor )
		{
			ShipLog.createNeronLog( SHIP_NO_MORE_ENGINE, s ).insert();
			ShipLog.createHeroLog( SHIP_NO_MORE_ENGINE_PILOT, h ).insert();
			return;
		}
		
		//push planet to visited list
		var curPlanet : DbPlanet = s.data.curPlanet;
		
		//cleanup prev
		s.data.curPlanet = null;
		
		var nextPlanet = null;
		//try jump and hook planet
		var or = Type.createEnumIndex( Orientation,s.currentOrientation);
		for(d in curPlanetList) {
			var pf = d.fuel;
			var po = d.dir;
			
			if( po == or && pf == s.enginePipeline)
			{
				s.data.curPlanet = (nextPlanet=d);
				break;
			}
		}
		
		if ( nextPlanet != null) {
			
			var oh = s.searchLiveTpl( KIM_JIN_SU , true );
			if ( oh != null && !oh.isMush() )	
				{
					if( Solver.decreasingProductivity( s.counter_planed_shiped, 6))
						oh.triumph( FAST_FORWARD_2 );
					
				}
				
			s.counter_planed_shiped++;
		}
	
		//cleanup scans
		hlist.iter( cleanupScan );
		
		if( nextPlanet != null )
		{
			var curSeed = nextPlanet.seed;
			var visList :List<Int> = NekoEx.unserialize(s.exploredList);
			if (visList == null)
				visList = new List<Int>();

			visList.add(curSeed);
			s.exploredList = NekoEx.serialize( visList );
			
			ShipLog.createNeronLog( NEW_PLANET, s, Data(nextPlanet.data.name) ).insert();
			ShipLog.createAdminHeroLog( ADM_NEW_PLANET, h, Data(nextPlanet.data.name) ).insert();
			
			db.Triumph.shipGrant( s, NEW_PLANET );
		}
		
		for( x in s.data.com.rebelData )
			if ( 	s.currentCycle >= x.signalStart
			&&		s.currentCycle <= x.signalEnd
			&&		!x.decoded)
				x.lost = true;
				
		var act = Hunter.manager.getActives(s,true);
		for(x in act )
		{
			if ( x.getType() == TRAX ) continue; //is not discarded
			
			s.aggroPool += x.getData().aggro_pool_cost; //back to pool
			
			if ( 	x.getType() == DICE
			|| 		x.getType() == ASTEROID
			|| 		x.getType() == TRANSPORT
			) //some are never pooled back
			{
				x.deleted = true;
				x.pooled = false;
			}
			else
			{
				x.state = HunterState.Stalled;
				x.pooled = true;
			}
			x.update();
		}
		s.recache();

		//cleanup old planet
		for( r in s.roomData)
		{
			var rt = Protocol.roomType( r.id );
			
			if( rt.id == PLANET)
			{
				ShipLog.manager.deleteRoomLog( s, r.id );
				for(x in r.inventory.array())
				{
					//clear missing, kill cats etc...
					if( !x.status.has(EQUIPMENT) )
						s.dmgObject2(  {item:x,rid:r.id,inv:r.inventory} );
				}
				r.inventory.d = [];
			}
			
			if ( 	r.status.has(DETACHED) 
			&&		rt.id == PATROL_SHIP )
			{
				var patrol : db.Patrol = s.getPatrol( r.id ,true);
				if ( patrol == null) {
					App.current.logError( "wtf patrol err" );
				}
				else {
					if ( s.isProjectUnlocked( MAGNETIC_NET ) && !patrol.isDestroyed() && s.neronVal(NB_MAGNET)==0) {
						for ( h in patrol.heroes()) 
							if( h != null)
								HeroLogic.patrolLand( patrol,h );
					}
					else { 
						for ( h in patrol.heroes()) 
							if( h != null)
								killMember( h, DC_ABANDONNED );
						patrol.hurt(1000);
					}
				}
			}
			
			if ( rt.id == OUTER_SPACE || rt.id == PLANET )
				for ( h in ShipLogic.listHeroesInRoom( s, r.id , true ))
					killMember( h, DC_ABANDONNED );
			
			if(rt.id == OUTER_SPACE)
				r.inventory.d = r.inventory.filter( function(i) return i.status.has(EQUIPMENT) ).array();
		}
		
		//clear values
		s.enginePipeline = 0;
		s.data.lastTravel = { date:s.currentCycle, duration:1 };
		s.update();
		
		//advertise
		ShipLog.createHeroLog( SHIP_LOCAL_ADVANCE, h ).insert();
		ShipLog.createNeronLog( leaveOrbit ? SHIP_CHANGED_ORBIT : SHIP_MOVED, s, Qty(s.data.lastTravel.duration) ).insert();
		
		new AdminStream( s, " jumped at " + s.currentCycle+" planet :" + curPlanet);
		s.heroes(true).iter( function(u) u.update() );
	}
	
	public static function killLink( s : Ship )
	{
		if ( s.data.com.isLinked )
		{
			s.data.com.isLinked = false;
			ShipLog.createNeronLog(SHIP_NO_MORE_COM, s).insert();
		}
	}
	
	public static function hurtHull( ship : db.Ship ,  d ) : Bool
	{
		var armor = ship.armor;
		
		var armoured = false;
		if ( ship.ruleFlags.has( Rule.ARMOUR_CORRIDOR ) )
			armoured = true;
		
		if(armoured)
			armor  += Gameplay.ARMOUR_CORRIDOR;
		
		d -= armor;
		if ( d <= 0 ) return false;
		
		hurt( ship, d );
		return true;
	}
	
	static function hurt( s : Ship,d:Int )
	{
		s.hp -= d;
		
		if( s.hp <= 0 && !s.destroyed )
		{
			new ShipLogic(s).destroyShip( DC_SHIP_DESTRUCTION );
		}
	}
	
	
	public function destroyShip( ?cause : DeathId, force = false)
	{
		mt.gx.Debug.assert( ship.isLocked() );
		var mTrace = function(s)
		{
			//ShipLog.createShipLog( CHAT, ship, ChatEntry( s )).set(Admin).insert();
		};
		
		mTrace("trying to destroy ship");
		if( ship.destroyed && !force)
		{
			mTrace("already destroyed :)");
			return;
		}
		
		var allHeroes = ship.heroes( true );
		
		if(cause == null) cause = DC_SHIP_DESTRUCTION;
		
		mTrace("destroyed destr:" + ship.destroyed);
		
		ship.destroyed = true;
		ship.destructionDate = Date.now();
		ship.canMaintain = false;
		ship.data.dc = cause;
		
		if( cause == DC_SHIP_DESTRUCTION)
			ship.cmTriumph(CM_DAEDALUS_EXPLODE);
		
		mTrace("all: " + allHeroes.length + " destr:" + ship.destroyed);
		if ( cause == DC_ARMAGEDDON) db.Triumph.shipGrant( ship, SUPER_NOVA );
		
		var thisDay = ship.getCurrentDay();
		var dayGt10 = (ship.group == null) && ship.getCurrentDay() >= Const.SUPER_GAME_DAYS;
		
		for( x in allHeroes)
		{
			var u = x.user;
				if ( !x.isMush() && dayGt10) {
					u.lock();
						u.paidGroupTicket++;
						u.notify( Text.epic_group_reward( { days:thisDay + 1 } ));
					u.update();
				}
			
			if ( !x.isDead() )
			{
				mTrace("killing: " + x.getHeroId());
				
				if ( cause == DC_DAEDALUS_GONE_TO_EDEN) {
					if ( x.isMush())	x.goal( Goal.id.eden_contaminated );
					else 				x.goal( Goal.id.eden );
				}
				
				killMember( x, cause);
			}
			
			if ( x.user != null)
			{
				if ( !x.pFlags.has( HAS_SLEEPED_ONCE ) && ship.getCurrentDay() > 5)
					x.goal( com.Goal.id.game_without_sleep );
				x.user.onShipDeath();
				x.user.update();
			}
			
			x.update();
		}
		ship.canMaintain = false;
		if ( ship.group != null)
		{
			ship.group.lock();
			ship.group.xp += Const.GRP_XP_PER_FLY;
			ship.group.update();
		}
		mTrace("updateing destr:" + ship.destroyed);
		ship.update();
		mTrace("historising");
				
		new db.HistoryShip(ship,cause).insert();
		mTrace("finished destr:" + ship.destroyed+" " + CallStack.callStack().join("<br/>"));
	}
	
	
	public static function unitTest() : Void
	{
		/*
		var res = "";
		var p =  PlanetSolver.findHostilePlanet( 3.1, 3.9);
		res += Std.string(p);
		
		var heroes = LambdaEx.randomSubset( Lambda.array(Ship.manager.heroes(hero.ship, false, false)), 1 );
		
		heroes.first().flags.unset(HeroFlags.HURT);
		heroes.first().flags.unset(HeroFlags.DEAD);
		
		evalExpeEvents( heroes, hero.ship,p );
		evalExpeResult( heroes, p);
		
		Debug.ASSERT( heroes.first().flags.has(HeroFlags.HURT) == true );
		Debug.ASSERT( heroes.first().flags.has(DEAD) == false );
		
		
		heroes.first().flags.unset(HeroFlags.HURT);
		heroes.first().flags.unset(HeroFlags.DEAD);
		*/
	}
	
	public static function getDualDoor( ship:Ship, here : RoomId, x : ItemDesc ) : Pair<ItemLocation,ItemLocation>
	{
		var  pair = new Pair( null, null);
		
		pair.first = { item:x, inv:ship.getRoom( here).inventory, rid:here };
		
		var doorDesc = getDoor( ship, x.getDoorId() );
		var to = (doorDesc.link[0].id == here) ? doorDesc.link[1].id : doorDesc.link[0].id;
		
		var t = ship.getRoom(to);
		for(di in t.inventory)
		{
			switch(di.id)
			{
				case DOOR:
				default: continue;
			}
			
			var did = di.getDoorId();
			var door = getDoor( ship,did);
			
			if ( 	door.link[0].id == here
			||		door.link[0].id == to)
			{
				if ( 	door.link[1].id == here
				||		door.link[1].id == to)
				{
					pair.second = { item:di, inv:t.inventory, rid:to };
					
					return pair;
				}
			}
		}
		
		throw "assert no dual door " + "from:" + here + " to:" + to;
		return null;
	}
	
	public static function calcPhysAdjRooms( ship : Ship,inR : RoomId, valid : RoomInfos -> Bool) : List<RoomInfos>
	{
		var doors = ship.getMap().rooms.find( function(r) return r.id == inR ).doors;
		
		var r = doors.map( 	function(r) return ship.roomData.get( ( r.link[0].id == inR) ? r.link[1].id : r.link[0].id ))
		.nullStrip();
		
		return r.filter( function(ri) return valid( ri ));
	}
	
	public static function getDoorPair( ship:Ship, from : RoomId,to:RoomId ) : Pair<ItemLocation,ItemLocation>
	{
		var pair = new Pair(null,null);
		
		var f = ship.getRoom(from);
		for(di in f.inventory)
		{
			var did = di.getDoorId();
			if( did != null )
			{
				var door = getDoor( ship,did);
				
				if ( 	door.link[0].id == from
				||		door.link[0].id == to)
				{
					if ( 	door.link[1].id == from
					||		door.link[1].id == to)
					{
						pair.first = { item:di, inv:f.inventory, rid:from };
						break;
					}
				}
			}
		}
		f = null;
		var t = ship.getRoom(to);
		for(di1 in t.inventory)
		{
			var did = di1.getDoorId();
			if( did != null )
			{
				var door = getDoor( ship,did);
				
				if ( 	door.link[0].id == from
				||		door.link[0].id == to)
				{
					if ( 	door.link[1].id == from
					||		door.link[1].id == to)
					{
						pair.second = { item:di1, inv:t.inventory, rid:to };
						return pair;
					}
				}
			}
		}
		
		return null;
	}
	
	public static function getDoor(ship:Ship,id)
	{
		for ( door in ship.getMap().doors ) {
			if ( door.id == id )
				return door;
		}
		return null;
	}
	
	public static function getDoorParam( i : { customInfos:List<ItemInfos>} , s : Ship,rid : RoomId ) : RoomId
	{
		var map = s.getMap();
		for ( x in i.customInfos  )
		{
			switch(x)
			{
				case Door( d ):
				if (  map.doors[d].link[0].id == rid )
					return  map.doors[d].link[1].id;
				else
					return  map.doors[d].link[0].id;
				
				default:
			}
		}
		return null;
	}
	
	
	
	public static function corruptItem(ship:Ship,item: ItemDesc )
	{
		
		if ( item.status.has( FOOD_UNSTABLE ))
		{
			if ( item.status.has( FOOD_FROZEN ) ) return;
			
			item.status.unset( FOOD_UNSTABLE );
			item.status.set( FOOD_HAZARDOUS );
		}
		else if ( item.status.has( FOOD_HAZARDOUS ))
		{
			if ( item.status.has( FOOD_FROZEN ) ) return;
			
			item.status.unset( FOOD_HAZARDOUS );
			item.status.set( FOOD_DECAYING );
		}
		else if( item.status.has( FOOD_DECAYING ))//do nothing gyurk
		{
			//GYYAAAA
		}
		else
		{
			item.status.set( FOOD_UNSTABLE );
		}
	}
	
	public static function launchShip( ?conf:EnumFlags<GameConf> ) : db.Ship {
		var p = new Ship(conf);
		p.postInit();
		return p;
	}
	
	public static function calcHeroList( user : db.User, ship: Ship ) : List<HeroId>
	{
		var rd = new neko.Random();
		var st = user.stats(false);
		var seed = user.id +  (16884) * st.nbGameStarted;
		
		rd.setSeed(  seed );
		
		var fulllist : List<HeroId> = Level.getCast().list();
		//if ( ship != null ) fulllist = ship.getCast().list();
		
		var proposal : List<HeroId> = new List();
		
		if( ship != null)
		{
			if ( ship.destroyed
			|| !ship.acceptsPlayers ){
				return new List();
			}
			
			var heroes = ship.heroes( false );
			
			for(h in fulllist )
				if ( !ship.heroes(false).test(function(hh) return hh.heroId == h) )
					proposal.push( h );
					
		}
		else proposal = fulllist.list();
		
		if ( ship != null)
			if( user.tutoDone == false || st.nbGameStarted < 5 )
				if ( proposal.length > 2 )
					proposal = proposal.filter( function(h) return !ship.data.subjectZero.has( h ) );
				
		proposal = proposal.scramble( rd ).tail(4);
		
		return proposal;
	}
	
	public static function addToCrew( u : User, ship : Ship , h : HeroId )
	{
		if ( !ship.isLocked() ) ship.lock();
		mt.gx.Debug.assert( u.isLocked() );
		
		Debug.ASSERT( u != null );
		Debug.ASSERT( ship != null );
		ship.recache();
		
		var tpl = ship.searchTpl( h, true );
		if ( tpl != null ) throw handler.Handler.HandlerAction.ActError("/", Text.embark_gloups);
		
		var tpl2 = Hero.manager.select( $ship == ship && $charTplId == h.index(), true );
		if ( tpl2 != null ) throw handler.Handler.HandlerAction.ActError("/", Text.embark_gloups);
			
		var hero = new Hero( u, h , ship  );
		hero.insert();
		
		if( hero.loc() == null) hero.location = RoomId.CORRIDOR_A;
		
		hero.postInit();
		new ShipLogic( ship ).embarkGroupMember( hero );
		
		if(hero.loc() == null)
		{
			hero.location = ship.roomData.first().id;
			Debug.ASSERT( hero.location !=  null );
		}
		
		switch(hero.heroId)
		{
			case RALUCA_TOMESCU:
			{
				var p = new db.PNJ( ship, Cat );
				ShipLog.createRoomLog( CAT_AWAKEN,ship, p.location ).insert();
			}
			default:
		}
		
		if ( !u.tutoDone )
			hero.startupTutoData();
		else
		{
			if ( !hero.user.hasPaid )
			{
				var tk = Utils.itemDesc( ship, TALKY_WALKY );
				tk.customInfos.push( Reserved( hero.heroId) );
				HeroLogic.createItem( hero, tk );
				
				var tck = Utils.itemDesc( ship, TRACKER );
				tck.customInfos.push( Reserved( hero.heroId) );
				HeroLogic.createItem( hero, tck);
			}
			else
			{
				var tck = Utils.itemDesc( ship, SUPER_TALKY );
				tck.customInfos.push( Reserved( hero.heroId) );
				HeroLogic.createItem( hero, tck);
			}
		}
		
		u.addHero( hero , true);
		
		if ( ship.inConf.has( NO_MUSH )) 			hero.goal(Goal.id.no_mush);
		if ( ship.inConf.has( THREE_MUSH )) 		hero.goal(Goal.id.three_mush);
		if ( ship.inConf.has( FAST_CYCLE )) 		hero.goal(Goal.id.fast_cycle);
		if ( ship.inConf.has( BLITZ_CYCLE )) 		hero.goal(Goal.id.blitz_cycle);
		if ( ship.inConf.has( SLOW_CYCLE )) 		hero.goal(Goal.id.slow_cycle);
		if ( ship.inConf.has( NO_MUSH_BAR )) 		hero.goal(Goal.id.no_mush_bar);
		if ( ship.inConf.has( MUSH_PROGRESSIVE )) 	hero.goal(Goal.id.mush_progressive);
		
		#if false
		if ( ship.inConf.has( FUNGAL_BATTLE )) 	hero.goal(Goal.id.fungal_battle);
		#end
		
		ship.staticMaintain();
		
		if(!hero.isMush())
			hero.notify( Text.welcome);
		else
			hero.notify( Text.welcome_mush );
		
		ship.groupScript( hero );
		
		u.update();
		ship.update();
		
			for( h in ship.heroes( true ) )
				if( !h.isMulti && h.hasMultisInGame() && !h.user.autorizeMulti ) {
					h.isMulti = true;
					h.update();
				}
		
		hero.update();
		
		return hero;
	}
	
	public static function getLinkedRoomsA( ship : Ship, place:  RoomId ) : Array<RoomInfos> {
		var l = getLinkedRooms(ship,place).array();
		l.sort( function(r0, r1) {
			return r0.id.index() - r1.id.index();
		});
		return l;
	}
	
	public static function getLinkedRooms( ship : Ship, place:  RoomId ) : List<RoomInfos> {
		var doorList = new List();
		
		var room = ship.getMap().rooms.find( function(rsd) return rsd.id == place );
		var loc = ship.getRoom( place );
		for(d in room.doors )
		{
			var corId = (d.link[1].id != place) ? d.link[1].id :d.link[0].id;
			var roomDesc = ship.getRoom( corId );
			
			for( obj in  loc.inventory )
			{
				if ( obj.id == DOOR && !obj.status.has(BROKEN) )
				{
					var prm = getDoorParam(obj, ship, place);
					if( prm == corId )
						doorList.pushBack( roomDesc );
				}
			}
		}
		
		return doorList;
	}
	
	public static function dmgObject2( s:Ship, oi : ItemLocation) : Void
	{
		dmgObject( s, oi, oi.rid, false );
	}
	
	//returns true whether the object should be detroyed
	public static function dmgObject( s:Ship, oi : ItemLocation, r : RoomId,  adv : Bool = false) : Void
	{
		mt.gx.Debug.assert(r != null);
		
		var o = oi.item;
		var inv = oi.inv;
		
		if ( o == null ) return;
		if ( o.status.has(BROKEN) ) return;
		if ( (Protocol.itemDb( o.id ).rep == 0) && !o.status.has( DESTRUCTIBLE )) return;
		
		o.status.set(BROKEN);
		
		if ( o.status.has(DESTRUCTIBLE) )
		{
			var subadv = onDestroyObject(s, o, r);
			inv.remove( o );
			
			if ( !subadv && adv)
				ShipLog.createRoomLog(OBJECT_DESTROYED, s, r, o.pack() ).insert();
		}
		else
			if( adv )
				ShipLog.createRoomLog(OBJECT_BROKEN, s,r, o.pack() ).insert();
		
		
		switch(o.id)
		{
			case DOOR:
				var pair = ShipLogic.getDualDoor(s, r, o);
				
				if( pair.first.item.status.has( BROKEN ))
					pair.second.item.status.set(BROKEN)
				else
					pair.first.item.status.set( BROKEN );
					
					
			//dmg the command as well
			case PATROL_INTERFACE:
			{
				var pi = ActionLogic.getPatrol(s, o);
				mt.gx.Debug.assert( pi != null, 'no patrol for patrol interface');
				
				var rm = pi.getRoom();
				var com = rm.inventory.find( function( i ) return i.id == PATROL_COMMAND || i.id == PASIPHAE_COMMAND );
				
				if( !com.status.has(BROKEN))
					dmgObject(s, {item:com, rid:rm.id,inv:rm.inventory}, pi.getRoomId(), false);
			}
			
			case PATROL_COMMAND,PASIPHAE_COMMAND:
			{
				var pi = ActionLogic.getPatrolInterface(s, oi.rid );
				mt.gx.Debug.assert( pi != null, 'no interface for patrol '+r);
				
				if ( !pi.item.status.has(BROKEN) )
					dmgObject(s, pi, pi.rid, false);
			}
				
			case COM_CENTER :		ShipLogic.killLink( s );
			case BODY_CAT:			PNJ.catDeath( s );
			case SOFA:
				var hr = ShipLogic.listHeroesInRoom( s, r, false);
				for (h in hr ) h.touch();
				
			case TALKY_WALKY:
				for ( h in s.liveHeroes( false ))
					if ( !h.hasActionBroadcastMessage())
						h.tickPM();
				
			default:
				
		}
		
		if(o.status.has(WORLD))
		{
			var rl = Protocol.worldItem2Rules( o.id );
			if( rl !=null)
				s.ruleFlags.unset( rl );
		}
		
		s.dirty = true;
	}
	
	/**
	 *
	 * @return true if a log was emitted
	 */
	public static function onDestroyObject( s:Ship, o : ItemDesc ,r : RoomId) : Bool
	{
		switch(o.id)
		{
			case LIFE_CAPSULE: 		killMember( s.searchTpl( o.getBodyParam().hid, true ), DC_SCHRODINGER_BOX );
			case INSECTOID_SHELL:
			{
				ShipLog.createRoomLog( EVT_INSECT_SHELL_EXPLODE, s, r, Item( o.id) ).insert();
				setFire( s, r);
				return true;
			}
			default:
		}
		return false;
	}
	
	public static function isPackaged(  i : ItemDesc, s: Ship )
		return rawEffects( i, s ).has( PACKAGED );
	
		
	//misses all side effects induce by gameplay see ActionLogic.consum**
	public static function rawEffects(i : ItemDesc, s: Ship ) : Array<ConsumableEffectType>
	{
		var base : Array<ConsumableEffectType> = [];
		var skin = ItemUtils.getSkin( i );
		if ( skin == null ) return base;
		
		var skinVal = skin.v;
		var skinType = skin.t;
		
		switch(skinType)
		{
			case SK_RATION: 	base = Protocol.rations[skinVal].effect.copy();
			case SK_FRUIT: 		base = s.data.fruits[skinVal].copy();
			case SK_DRUG: 		base = s.data.drugs[skinVal].copy();
			case SK_PLANT: //
		}
		
		return base;
	}
	
	public function onNewDroneCycle( drones : Iterable<ItemLocation> )
	{
		var s = ship;
		for ( d in drones)
		{
			var dInfos = d.item.getDroneInfos();
			new DroneLogic( d, s ).tick();
			if( dInfos.pawa.has(DPU_ENHANCED))
				if( Dice.percent(80))
				{
					ShipLog.createRoomLog(DRONE_PAID, s,d.rid, DroneName( dInfos.name ) ).insert();
					new DroneLogic( d, s ).tick();
				}
		}
	}

	public function setToSol()
	{
		var alllH = ship.liveHeroes(true);
				
		var allMush = alllH.filter( function(h) return h.isMush() );
		var hr = alllH.filter( function(h) return !h.isMush() );
		var nbMush = allMush.length;
		
		db.Triumph.shipGrant( ship, RETURN_TO_SOL);
		
		//set triumph
		for (i in 0...nbMush)
			db.Triumph.shipGrant( ship, SOL_MUSH_INTRUDER );
		
		for (r in Protocol.researchList )
			if ( ship.isResearchUnlocked( r.id ) && r.glory!=null)
				db.Triumph.shipGrant( ship, r.glory );
				
		for ( h in allMush)
			h.triumph( SOL_MUSH_INVASION, TRIUMPH_EARNED );

		//give also to mushes
		ship.goal( com.Goal.id.back_to_root );
		
		for (h in alllH)
			h.update();
		
		destroyShip( DC_DAEDALUS_BACK_TO_SOL);
	}
	
	public function setToEden()
	{
		var allH : List<db.Hero> = ship.liveHeroes(true);
		var allMush = allH.filter( function(h) return h.isMush() );
		var hr = allH.filter( function(h) return !h.isMush() );
		var nbMush = allMush.length;
		var hasMale = allH.test( function(h) return h.isMale);
		var hasFemale = allH.test( function(h) return !h.isMale);
		var ian = allH.find( function(h) return h.heroId == IAN_SOULTON);
		var jinSu = allH.find( function(h) return h.heroId == KIM_JIN_SU);
		var chun = allH.find( function(h) return h.heroId == ZHONG_CHUN);
		var cat = db.PNJ.manager.search( $ship == ship && !$dead, true )
		.filter( function(pnj) return pnj.isCat()).first();
		
		if ( hasMale && hasFemale ) db.Triumph.shipGrant( ship, EDEN_SEXY); 
		
		
		
		var omap = ship.objectMap();
		
		var plants = 0;
		
		for ( o  in omap )
			if ( o.item.id == TREE_POT )
			{
				var sk = o.item.getSkin();
				if ( sk != null)
					plants |= 1<<sk.v;
			}
		
		db.Triumph.shipGrant( ship, EDEN_ONE_MAN, allH.length );
		
		for ( i in 0...MathEx.countBits( plants ))
		{
			db.Triumph.shipGrant( ship, EDEN_ALIEN_PLANT); 
			if ( ian != null) ian.triumph(EDEN_ALIEN_PLANT_PLUS);
		}
		
		if ( cat != null )
		{
			if (cat.isMush()) db.Triumph.shipGrant( ship, EDEN_MUSH_CAT );
			else  db.Triumph.shipGrant( ship, EDEN_CAT );
		}
		else 	
			db.Triumph.shipGrant( ship, EDEN_NO_CAT );
		
		var nbDis = allH.count( function(h) return h.data.diseases.count() > 0 );
		
		for ( i in 0...nbDis) db.Triumph.shipGrant( ship, EDEN_MICROBES );
		for ( h in allH)
		{
			if ( h.skillsHas( ENGINEER) ) h.triumph(EDEN_ENGINEERS);
			if ( h.skillsHas( BIOLOGIST) || h.skillsHas(MEDIC) ) h.triumph(EDEN_BIOLOGISTS);
		}
		
		for ( h in allMush )
		{
			h.triumph( EDEN_MUSH_INVASION );
			db.Triumph.shipGrant( ship,EDEN_MUSH_INTRUDER );
		}
		
		for ( h in hr) {
			if ( h.isPregnant())  ship.triumph( PREGNANT_IN_EDEN );
		}
				
		if (jinSu != null) jinSu.triumph( SAVIOR );
		if (chun != null) chun.triumph( REMEDY );
		
		for (r in Protocol.researchList )
			if ( ship.isResearchUnlocked( r.id ) && r.glory!=null)
				db.Triumph.shipGrant( ship, r.glory );
				
		for (h in allH)
			h.update();
		destroyShip( DC_DAEDALUS_GONE_TO_EDEN);
	}
	
	public function getActivityLevel() : Float
	{
		var zzz = 0;
		var lh = ship.heroes(false);
		for ( i in lh)
			if ( i.flags.has(SLEEPY))
				zzz++;
		if ( lh.length == 0 ) return 0.0;
		return (lh.length - zzz) / lh.length;
	}	
	
	public static var FDS_ELIM :Array<EventId> = [PA_UP, XP_UP, ADM_EPITAPH, TIME_CHANGE, MORAL_UP, ONE_MORE_DAY, 
	FRUIT_MATURED, REBEL_SIGNAL_DETECTED, TRIUMPH_PERSONAL_ACCOMPLISHMENT, ADM_PRIVATE, 
	INSPECT, DISABLED_HELPED, WAKE_UP_FROM_SLEEP, FALLS_ASLEEP,
	LOG_LIST_CREW, CUDDLE_CAT, GRENADE_ATTACK, FIERY_SPEECH,
	WAKE_UP_FROM_SLEEP,EXEC_GP_ACTION,TIME_CHANGE];
	
	public static var FDS_GP_ELIM :Array<ActionId>= [MOVE];
	
	public function mkHistory( ?curH : db.Hero, logs : List<ShipLog>, fds = false )
	{
		var res = new List();
		
		for ( x in logs)
		{
			function mk(t,cl="") 	res.pushBack( {txt:t, when: Utils.formatCycle(x.cycle), cl:cl } );
			function rp( t) 		return "<strong>" + t + "</strong>";
			
			function acUsed(x:ShipLog) {
				if (!fds) return;
				
				var lg = x.contentCache;
				var ev = EventId.createI( x.event );
				var tip = "";
				var ac = null;
				if ( ev == EXEC_GP_ACTION ||ev == EXEC_ACTION ) {
					switch( x.dataAccess) {
						default:
						case ActionUsed( a, r ):
							ac = a;
							if ( !FDS_GP_ELIM.has( a ))
							{	
								lg = Text.ac_done +" : "+ Protocol.actionDb(a).name;
								tip += '[AC:' + a + ']';
								if ( r != null) {
									if ( r.tgtItem != null) 	tip += "[TGT="+ActionLogic.resolveItemName(r.tgtItem)+"]";
									if ( r.tgtEntity != null) 	tip += "[TGT="+r.tgtEntity+"]";
									if ( r.tgtHero != null) 	tip += "[TGT="+ Protocol.heroesDb(r.tgtHero).surname+"]";
								}
							}
							else 
							return;
					}
				}
				
				if( x.flags.has(Admin ))
					tip += '[ADM] ';
					
				if ( x.room != null)  tip += '[ROOM:'+Protocol.roomDb( x.room).name+']';
					
				tip += '[EV:' + EventId.createI(x.event) + ']';
				
				var t = new Tag('span')
				.tip(tip)
				.content(lg);
				
				if ( ac != null) if ( Protocol.actionDb( ac ).aggro ) t.css('color', 'red');
				lg =  t.toString();
				mk(lg);
			}
			
			switch( EventId.createI( x.event ))
			{
				default:
					if(fds) acUsed(x);
				//skip it
				case DEATH:
					//if ( x.heroId == id)
					//	mk(Protocol.deathList[x.deathView().index()].short_desc);
						
				case RESEARCH_PROGRESSED: 	if( curH!=null&&x.heroId == curH.id )				mk(Gen.TXT.generate("gmu_research_done").replace("#a", rp(Protocol.researchDb(x.researchView()).name )));
				case PROJECT_COMPLETE:		if ( curH!=null&&x.heroId == curH.id )				mk(Gen.TXT.generate("gmu_project_done").replace( "#a", rp(Protocol.projectDb(x.projectView()).name )));
				case ADM_ASSASSINATION:
					var who = x.heroView();
					var ent = x.entityView();//string
					
					if ( curH!= null && x.hid() == curH.heroId)
						mk(Gen.TXT.generate("gmu_assassinator").replace( "#a", rp(ent)));
					else
					{
						if ( who == curH )
							mk(Gen.TXT.generate("gmu_assassinated").replace( "#a", rp(x.readHero() )));
					}
						
				case ADM_MUSH_INITIAL:
						if ( curH!= null && x.heroId == curH.id ) mk(Gen.TXT.generate("gmu_mush_init"), "became");
						
				case ADM_MUSH_CONVERTED:
					var who = x.subjectView();
					
					if ( curH != null ){
						if ( x.heroId == curH.id )
							if ( who == null ) mk(Gen.TXT.generate("gmu_mush_conv"), "became");
							else mk(Gen.TXT.generate("gmu_mush_conv_by").replace( "#a", rp( Utils.buildFirstName(who))), "became");
						else if ( who == curH.heroId )
							mk(Gen.TXT.generate("gmu_mush_converter").replace( "#a", rp(Utils.buildFirstName(HeroId.createI(x.heroCharId)))), "became");
					}
						
				case EXEC_ACTION:
					if ( curH != null && x.heroId == curH.id ) {
						var processed = false;
						switch( x.dataAccess )
						{
							default: //if(fds) mk('anomaly');
							case ActionUsed( acId, r ):
							{
								var acd = Protocol.actionDb( acId );
								
								function proc(s:String):String
								{
									if ( r != null)
									{
										var h = new Hash();
										
										if( r.tgtHero != null)
											h.set("#h", "*" + Utils.buildFirstName( r.tgtHero) + "*");
											
										if( r.tgtEntity != null)
											h.set("#e", "*"+r.tgtEntity+"*");
											
										if ( r.tgtItem != null)
										{
											var itn = ActionLogic.resolveItemName( r.tgtItem);
											var itg = ActionLogic.resolveItemGender( r.tgtItem );
											h.set("#i",  	"*" + itn + "*");
											
											h.set("#un_i",  Locale.art_un(itn, itg, "*"));
											h.set("#en_a_i", Locale.art_en_a(itn, itg, "*"));
											h.set("#es_un_i",Locale.art_es_un(itn, itg, "*"));
											
											h.set("#de_i",  Locale.art_de(itn, itg, "*"));
											h.set("#es_del_i", Locale.art_es_del(itn, itg, "*"));
											
											h.set("#du_i",  Locale.art_du(itn,itg,"*"));
											h.set("#le_i",  Locale.art_le(itn,itg,"*") );
										}
										
										var rn = Protocol.roomDb( x.room );
										h.set( "#r",  rn.name);
										h.set( "#au_r", Locale.art_au( rn.name,rn.gender,"*"));
										
										for ( k in h.keys() )
										{
											var v = h.get(k);
											s = s.replace( k, v );
										}
									}
									return TextEx.quickFormat(s);
								}
								
								if ( r != null)
								{
									if ( r.success == null)
									{
										if( acd.gmu_ok.length>0)
											mk( proc(acd.gmu_ok.random()) );
									}
									else
									if ( r.success == false )
									{
										if ( acd.gmu_failed.length>0)
											mk( proc(acd.gmu_failed.random()) );
									}
									else
									{
										if ( acd.gmu_ok.length>0)
											mk( proc(acd.gmu_ok.random()) );
									}
								}
							}
						}
						if(fds) acUsed(x);
					}
					else
					{
						switch( x.dataAccess )
						{
							default:
							case ActionUsed( acId, r ):
							{
								switch( acId ){
									case EXCHANGE_BODY: 
										if( curH!= null ){
											if( r.tgtHero == curH.heroId )
												mk( Text.corpse_exchange( { tgt:Utils.buildFirstName( HeroId.createI(x.heroCharId)  ) } ));
										}
									default:
								}
							}
						}
						
						if(fds) acUsed(x);
					}
			}
		}
		
		return res;
	}
	
}

