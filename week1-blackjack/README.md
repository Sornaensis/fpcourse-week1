INSTRUCTIONS

For this project we will implement a simple version of blackjack played between the player and dealer.

There are several expressions left as 'undefined' in Main.hs

The goal is to replace them with expressions that make the program work and follow the rules listed below.

A test suite will be added to this repo so that you may check your programs against it to ensure it is correct.

Project 1 Part 1 - BlackJack

Course of Gameplay:

1. Game Starts (New game with new deck of cards, shuffled)
2. Player places a bet and the dealer and player are both dealt cards alternately until they each have two cards.
3. The player chooses whether to receive additional cards (to 'Hit') or keep the cards and continue 'Stand'
    a. If the player takes an additional card and their hand value changes to 'Bust' they lose, and lose the amount they bet
    b. The player may keep taking cards until they choose to Stand or Bust
4a. If the player is Bust the game ends immediately, losing them the amount they bet
4b. Otherwise, the dealer reveals his hand value
    a. If the dealer's cards are worth less than 17, the dealer is dealt additional cards until they are at least worth 17
    b. If the player has blackjack and the dealer does not, the player wins
    c. If the dealer goes bust and the player does not, the player wins
    d. If the player and dealer both have blackjack it is a draw and no one wins
    e. If the player has a higher valued set of cards than the dealer, the player wins
5. If the player wins he is paid his bet back plus 100%
    a. For example if the player has 200 in cash and bets 50 and wins the player should have 300 in cash afterwards
    b. If the player wins with blackjack their winnings are multiplied by 1.5, so in the above example they would have 350 afterwards if they won with blackjack
6. If the player loses they only lose the amount bet
7. After winning, losing, or drawing, the player's money and Win/Loss/Draw data is updated and they may start a new game
