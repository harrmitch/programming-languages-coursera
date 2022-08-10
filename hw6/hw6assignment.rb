# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # class array holding all the pieces and their rotations
  All_My_Pieces = All_Pieces + [rotations([[0, 0], [1, 0], [0, -1]]), # square -1
               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]]), # square +1
               [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], # long +1 (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]]]

  # your enhancements here
  def self.next_piece (board, cheating=false)
    if cheating
      MyPiece.new([[[0, 0]]], board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheating = false
  end

  def cheat
    if @score >= 100 and !@cheating
      @score -= 100
      @cheating = true
    end
  end

  def next_piece
    @current_block = MyPiece.next_piece(self, @cheating)
    @current_pos = nil
    @cheating = false
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.length-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def key_bindings
    super
    @root.bind("u", proc { 2.times {@board.rotate_clockwise} })

    @root.bind("c", proc { @board.cheat })
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
end


