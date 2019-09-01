# Game Play

RoveR is consecutive turns taken by the player's rover and then all other entities. Each turn the player may activates a single module of the rover or wait. Activating modules or waiting consumes energy. When the player's rover has lost all energy or integrity it destructs, and the player loses the game. Destruction is permanent. 

The aim of RoveR is to explore planets, survive hostile encounters and gather data.

# Specifications

Specifications contribute to the success or failure of exploring rovers. Rovers can increase their maximum specifications by acquiring new chassis. Rovers increase or decrease specification values by using modules. 

**Energy** determine which modules a rover can use. Using modules or waiting consumes energy.  Using modules with generate abilities can restore energy to a rover. Returning to a drop pod restores a rover's energy to full.  A rover destructs if its energy declines to zero.

**Integrity** is the amount of damage that a rover can tolerate before destruction. Using modules with repair abilities can restore integrity to a rover. A rover destructs if its integrity declines to zero.

**Shielding** counteracts damage dealt to a rover. Shielding declines in proportion to the amount of damage it counteracts. Using modules with shield abilities can restore integrity to a rover. 

**Power** increases the damage of a rover's attacks during combat. 

**Sensitivity** determines the accuracy of a rover's modules during combat. Rovers with high sensitivity are more likely to hit targets and dodge attacks. 

**Data** determine a rover's ability to access facilities and use technologies. Using data corrupts it. Analyzing or destroying entities provides a rover with new data. 

# Modules

A rover may use any module they choose, if they can afford its e**nergy cost**. A rover can only use one module per turn. Installing or removing modules requires one turn. Modules have a fixed **durability** that declines with each use. When a module has lost all durability, it disintegrates. 

## **Module Abilities**

Modules have one or more abilities. When activated, a module uses each of its abilities. Module abilities may apply to the player's rover or may need a target entity. 

**Move** shifts a rover to an adjacent coordinate of the map. Rovers cannot displace a blocking entity while moving. Stalled entities cannot move. 

**Repair** restores integrity to a rover. Repair cannot revive rovers or restore more than a rover's maximum integrity. 

**Shield** adds shielding to a rover. Shield cannot restore more than a rover's largest shielding.

**Generate** restores energy to a rover. Generate cannot revive rovers or restore more than a rover's maximum energy. 

**Damage** deals damage to the integrity of a target entity. The power of a rover increases damage. The shielding of the target entity counteracts this damage.

**Analyze** collects data from a target entity. If the target entity lacks sufficient data, all data transfers from the target to the rover. Analyze cannot store more than a rover's maximum data. 

**Stall** prevents a target entity from moving. A stalled entity can use all other module abilities. Stall does not prevent teleportation. 

**Short** prevents target entities from using any module abilities except for movement.

**Teleport** exchanges the location of a rover and a target entity on the map. Stalled entities can teleport. 

## **Module Targeting **

Modules may apply solely to an acting rover or may apply to one or more target entities. The **range** of a module is the maximum distance over which a module can target an entity.  Each module belongs to a targeting class which determines the affected entities. 

**Transport modules** allow a rover to move to an adjacent coordinate of the map. Transport modules never miss and do not need a target entity. 

**Internal modules** apply their abilities solely to the acting rover. Internal modules never miss and do not need a target entity. 

**Burst** **modules** target all entities within range of the acting rover. Burst modules never miss and do not need a target entity. 

**Local modules** affect a neighbouring target entity. A rover activates a local module by moving toward an adjacent entity. A rover's local targeting can be toggled between hostile entities or all entities. A local module misses if a rover's sensitivity plus 1d20 is less than the sensitivity of the target.

**Ranged modules** target the first entity within range along a path from a rover. A rover determines the cardinal direction to direct a ranged module. A ranged module misses if a rover's sensitivity plus 1d12 is less than the sensitivity of the target.

**Piercing Modules** target any entity within range along a path from a rover. A rover determines the cardinal direction to direct a ranged module. A ranged module misses if a rover's sensitivity plus 1d6 is less than the sensitivity of a given target.

# Controls

             [W]   Move/Aim North

               [S]   Move/Aim South

              [D]   Move/Aim East

              [A]   Move/Aim West

               [1]   Select Module Slot One

               [2]   Select Module Slot Two

               [3]   Select Module Slot Three

       [Enter]   Activate Selected Module

              [L]   Toggle Hostile Targeting

# Credits

RoveR is by Foxfields. I welcome collaborators. Please get in touch if you want to make substantive contributions to this work.