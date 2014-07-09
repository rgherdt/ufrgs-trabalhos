library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity ctrl_unit is
port (clk         : in std_logic;
      reset       : in std_logic;
      flags_in    : in std_logic_vector(4 downto 0);
      dec_in      : in instdec_type;
      control_out : out ctlcod_type;
end ctrl_unit;

architecture ctrl_unit of ctrl_unit is

    component count3 is
    port (clk,ld : in std_logic;
          din : in std_logic_vector (2 downto 0);
          count : out std_logic_vector (2 downto 0));
    end component;

    signal couter_ld : std_logic;
    signal next_stage : std_logic_vector(2 downto 0);
    signal nflag, zflag, cflag, vflag, bflag : std_logic;
    signal decnop, decsta, declda, decadd, decor, decand, decnot, decsub,
           decshr, decshl, decror, decrol, decjmp, decjn, decjp, decjz, decjnz,
           decjc, decjnc, decjv, decjnv, decjb, decjnb, dechlt : std_logic;

    nflag  <= flags_in(0);
    zflag  <= flags_in(1);
    cflag  <= flags_in(2);
    vflag  <= flags_in(3);
    bflag  <= flags_in(4);
    decnop <= dec_in(0);
    decsta <= dec_in(1);
    declda <= dec_in(2);
    decadd <= dec_in(3);
    decor  <= dec_in(4);
    decand <= dec_in(5);
    decnod <= dec_in(6);
    decsub <= dec_in(7);
    decshr <= dec_in(8);
    decshl <= dec_in(9);
    decror <= dec_in(10);
    decrol <= dec_in(11);
    decjmp <= dec_in(12);
    decjn  <= dec_in(13);
    decjp  <= dec_in(14);
    decjz  <= dec_in(15);
    decjnz <= dec_in(16);
    decjc  <= dec_in(17);
    decjnc <= dec_in(18);
    decjv  <= dec_in(19);
    decjnv <= dec_in(20);
    decjb  <= dec_in(21);
    decjnb <= dec_in(22);
    dechlt <= dec_in(23);

    constant t0_cod : ctlcod_type := "000010000000000";
    constant t1_cod : ctlcod_type := "010010000000000";
    constant t2_cod : ctlcod_type := "000000001000000";
    constant t3_op  : ctlcod_type := "000010000000000";
    constant t3_not : ctlcod_type := "100000000000001";
    constant t3_brp : ctlcod_type := "010000000000000";
    constant t4_op  : ctlcod_type := "010001000000000";
    constant t4_br  : ctlcod_type := "000001000000000";
    constant t5_op  : ctlcod_type := "000110000000000";
    constant t5_br  : ctlcod_type := "001000000000000";
    constant t6_op  : ctlcod_type := "000001000000000";
    constant t6_sta : ctlcod_type := "000000010000000";
    constant t7_sta : ctlcod_type := "000000100000000";
    constant t7_lda : ctlcod_type := "100000000000001";
    constant t7_add : ctlcod_type := "100000000000001";
    constant t7_or  : ctlcod_type := "100000000000001";
    constant t7_and : ctlcod_type := "100000000000001";
    constant t7_sub : ctlcod_type := "100000000000001";

    signal aluop : aluop_type;
    signal alunop, alusta, alusta, alulda, aluadd, aluor, aluand, alunot,
           alusub alujmp, alujn, alujp, alujv, alujnv, alujz, alujnz, alujc,
           alujnc, alujb, alujnb, alushr, alushl, aluror, alurol, aluhlt;
    signal opflag, brflag : std_logic;

begin

    st_count : count3
    port map (clk   => clk,
              ld    => counter_ld,
              din   => PS;
              count => NS);

              
    uc : process (clk)
    begin
        if (reset = '1') then
            NS <= "000";
        elsif (rising_edge(clk)) then
            case PS is
                when "000" => control_out <= t0_cod;
                when "001" => control_out <= t1_cod;
                when "010" => control_out <= t2_cod;
                when "011" =>
                    if (decsta or declda or decand or decor or decand
                        or decsub or decjmp) then
                        control_out <= t3_op;
                    elsif (decnot) then
                        control_out <= t3_not;
                    elsif ((decjn and nflag ) or
                           (decjp and not nflag) or
                           (decjz and zflag) or
                           (decjnz and not zflag) or
                           (decjc and cflag) or
                           (decjnc and not cflag) or
                           (decjv and vflag) or
                           (decjnv and not vflag) or
                           (decjb and bflag) or
                           (decjnb and not bflag)) then
                        control_out <= t3_op;
                    elsif  (decnop or dechlt) then
                        PS <= "000";
                    else
                        control_out <= t3_brp;
                        PS <= "000";
                    end if;
                when "100" =>
                    if (decsta or declda or decand or decor or decand
                        or decsub) then
                        control_out <= t4_op;
                    else control_out <= t4_br;
                when "101" => control_out <= t2_cod;

