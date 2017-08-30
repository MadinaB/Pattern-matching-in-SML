# Pattern-matching-in-SML

Careful_player takes a card-list and a goal and returns a move-list such that calling officiate with the card-list, the goal, and the move-list has this behavior: 
• The value of the held cards never exceeds the goal. 
• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. 
• If a score of 0 is reached, there must be no more moves. 
• If it is possible to discard one card, then draw one card to produce a score of 0, then this must be done. (Note careful_player will have to look ahead to the next card, which in many card games is considered “cheating.”)
