DELETE FROM action 
  WHERE id = ?;
DELETE FROM action_bar 
  WHERE ship_id = ? AND action_id = ? AND position = ?;
DELETE FROM firing_arc 
  WHERE id = ?;
DELETE FROM huge_maneuver_dial 
  WHERE ship_id = ? AND speed = ? AND maneuver_id = ?;
DELETE FROM huge_ship 
  WHERE ship_id = ?;
DELETE FROM maneuver 
  WHERE id = ?;
DELETE FROM maneuver_dial 
  WHERE ship_id = ? AND speed = ? AND maneuver_id = ?;
DELETE FROM pilot 
  WHERE id = ?;
DELETE FROM ship 
  WHERE id = ?;
DELETE FROM ship_firing_arc 
  WHERE ship_id = ? AND firing_arc_id = ?;
DELETE FROM unique_name 
  WHERE id = ?;
DELETE FROM upgrade 
  WHERE id = ?;
DELETE FROM upgrade_bar 
  WHERE ship_id = ? AND upgrade_id = ? AND position = ?;

