library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

package ahmes_lib is
--    type aluop_type is (add_op, sub_op, or_op, and_op, not_op);
    type shift_type is (shift_r, shift_l, rotate_l, rotate_r);
    type branch_type is (jmp, jn, jp, jv, jnv, jz, jnz, jc, jnc, jb, jnb);
    type state_type is (nop, sta, lda, hlt);
    type memory_type is array (0 to 31) of std_logic_vector(7 downto 0);
    subtype ctlcod_type is std_logic_vector(20 downto 0);
    subtype instdec_type is std_logic_vector(23 downto 0);
    subtype aluop_type is std_logic_vector(5 downto 0);
    subtype bus8 is std_logic_vector(7 downto 0);

    constant NOPCOD : std_logic_vector(7 downto 0) := x"00";
    constant STACOD : std_logic_vector(7 downto 0) := x"10";
    constant LDACOD : std_logic_vector(7 downto 0) := x"20";
    constant ADDCOD : std_logic_vector(7 downto 0) := x"30";
    constant ORCOD  : std_logic_vector(7 downto 0) := x"40";
    constant ANDCOD : std_logic_vector(7 downto 0) := x"50";
    constant NOTCOD : std_logic_vector(7 downto 0) := x"60";
    constant SUBCOD : std_logic_vector(7 downto 0) := x"70";
    constant JMPCOD : std_logic_vector(7 downto 0) := x"80";
    constant JNCOD  : std_logic_vector(7 downto 0) := x"90";
    constant JPCOD  : std_logic_vector(7 downto 0) := x"94";
    constant JVCOD  : std_logic_vector(7 downto 0) := x"98";
    constant JNVCOD : std_logic_vector(7 downto 0) := x"9c";
    constant JZCOD  : std_logic_vector(7 downto 0) := x"a0";
    constant JNZCOD : std_logic_vector(7 downto 0) := x"a4";
    constant JCCOD  : std_logic_vector(7 downto 0) := x"b0";
    constant JNCCOD : std_logic_vector(7 downto 0) := x"b4";
    constant JBCOD  : std_logic_vector(7 downto 0) := x"b8";
    constant JNBCOD : std_logic_vector(7 downto 0) := x"bc";
    constant SHRCOD : std_logic_vector(7 downto 0) := x"e0";
    constant SHLCOD : std_logic_vector(7 downto 0) := x"e1";
    constant RORCOD : std_logic_vector(7 downto 0) := x"e2";
    constant ROLCOD : std_logic_vector(7 downto 0) := x"e3";
    constant HLTCOD : std_logic_vector(7 downto 0) := x"f0";
end ahmes_lib; 

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity reg8 is
port (reg_in : in bus8;
      ld, clk : in std_logic;
      reg_out : out bus8);
end reg8;

architecture reg8 of reg8 is
begin
    reg: process(clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then
            reg_out <= reg_in;
            end if;
        end if;
    end process;
end reg8;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity count8 is
port (clk,ld : in std_logic;
      din : in bus8;
      count : out bus8);
end count8;

architecture count8 of count8 is
    signal t_cnt : unsigned(7 downto 0); -- internal counter signal
begin
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then t_cnt <= unsigned(din); -- load
            else t_cnt <= t_cnt + 1; -- incr
            end if;
        end if;
    end process;
    count <= std_logic_vector(t_cnt);
end count8;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity count3 is
port (clk,ld : in std_logic;
      din : in std_logic_vector (2 downto 0);
      count : out std_logic_vector (2 downto 0));
end count3;

architecture count3 of count3 is
    signal t_cnt : unsigned(2 downto 0); -- internal counter signal
begin
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then t_cnt <= unsigned(din); -- load
            else t_cnt <= t_cnt + 1; -- incr
            end if;
        end if;
    end process;
    count <= std_logic_vector(t_cnt);
end count3;


library ieee;
use ieee.std_logic_1164.all;
use work.ahmes_lib.all;

entity mux16_8 is
port (clk, sel : in std_logic;
      mux_in0 : in bus8;
      mux_in1 : in bus8;
      mux_out : out bus8);
end mux16_8;

architecture mux16_8 of mux16_8 is
begin
    mux: process (clk)
    begin 
        if (rising_edge(clk)) then
            if (sel = '0') then mux_out <= mux_in0;
            else mux_out <= mux_in1;
            end if;
        end if;
    end process;
end mux16_8;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity shifter is
port (clk, ld, shl, shr, rol_flag, ror_flag, cflag_in : std_logic;
      din : in std_logic_vector(7 downto 0);
      dout : out std_logic_vector(7 downto 0);
      nflag, zflag, cflag: out std_logic);
end shifter;

architecture shifter of shifter is
    signal temp : unsigned(8 downto 0);
begin
    shifter: process(clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then
                temp(7 downto 0) <= unsigned(din);
            elsif (shl = '1') then
                temp <= shift_left(unsigned(cflag_in & din), 1);
            elsif (shr = '1') then
                temp <= shift_right(unsigned(din & cflag_in), 1);
            elsif (rol_flag = '1') then
                temp <= rotate_left(unsigned(cflag_in & din), 1);
            elsif (ror_flag = '1') then
                temp <= rotate_right(unsigned(cflag_in & din), 1);
            end if;
            if (temp = 0) then
                zflag <= '1';
                nflag <= '0';
            elsif (temp(0) = '1') then
                zflag <= '0';
                nflag <= '1';
            else
                zflag <= '0';
                nflag <= '0';
            end if;
            cflag  <= temp(0);
            dout   <= std_logic_vector(temp(8 downto 1));
        end if;
    end process;
end shifter;
                

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity ahmes is
port (clock: in std_logic;
      reset: in std_logic;
      accum: out std_logic_vector(7 downto 0));
end ahmes;

architecture ahmes of ahmes is

    component alu is
    port (x : in signed(7 downto 0);
          y : in signed(7 downto 0);
          alu_opsel : in std_logic_vector(5 downto 0);
          alu_out : out signed(7 downto 0);
          flags_out : out std_logic_vector(4 downto 0));
    end component;

    component ctrl_unit is
    port (clk         : in std_logic;
          reset       : in std_logic;
          flags_in    : in std_logic_vector(4 downto 0);
          dec_in      : in instdec_type;
          control_out : out ctlcod_type);
    end component;

    component datapath is
    port (clock       : in std_logic;
          control_in  : in ctlcod_type;
          flags_in    : in std_logic_vector(4 downto 0);
          flags_out   : out std_logic_vector(4 downto 0);
          accum_out   : out std_logic_vector(7 downto 0);
          dec_out     : out instdec_type;
          mem_in      : in bus8;
          mem_out     : out bus8;
          alu_res     : in signed(7 downto 0);
          alu_x       : out signed(7 downto 0);
          alu_y       : out signed(7 downto 0));
    end component;

    component memory is
    port (clk    : in std_logic;
          rem_in : in bus8;
          read, write : in std_logic;
          rdm_in : in bus8;
          rdm_out : out bus8);
    end component;


    signal mem : memory_type;
    signal alu_x, alu_y : bus8;
    signal alu_data : bus8;
    signal alu_opsel : std_logic_vector(5 downto 0);
    signal control  : ctlcod_type;
    signal instdec  : instdec_type;
    signal flags    : std_logic_vector(4 downto 0);
    signal alu_add, alu_or, alu_and, alu_not, alu_sub, alu_passy,
           ctl_shr, ctl_shl, ctl_ror, ctl_rol : std_logic;
    signal ac_ld, pc_inc, pc_ld, mpx_sel, rem_ld, mem_rd, mem_wr, rrdm_ld,
           wrdm_ld, ri_ld, flags_ld: std_logic;

    signal mem_in  : bus8;
    signal mem_out : bus8;

begin    

    ahmes_alu : alu
    port map (x => signed(alu_x),
              y => signed(alu_y),
              alu_opsel => alu_opsel,
              std_logic_vector(alu_out)   => alu_data,
              flags_out => flags);

    ahmes_ct : ctrl_unit
    port map (clk      => clock,
              reset    => reset,
              flags_in => flags,
              dec_in   => instdec,
              control_out => control);

    ahmes_dp : datapath
    port map (clock      => clock,
              control_in => control,
              flags_in   => flags,
              flags_out  => flags,
              accum_out  => accum,
              dec_out    => instdec,
              mem_in     => mem_out,
              mem_out    => mem_in,
              alu_res    => signed(alu_data),
              std_logic_vector(alu_x) => alu_x,
              std_logic_vector(alu_y) => alu_y);
    
              
end ahmes;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;


entity datapath is
port (clock       : in std_logic;
      control_in  : in ctlcod_type;
      flags_in    : in std_logic_vector(4 downto 0);
      flags_out   : out std_logic_vector(4 downto 0);
      accum_out   : out std_logic_vector(7 downto 0);
      dec_out     : out instdec_type;
      mem_in      : in bus8;
      mem_out     : out bus8;
      alu_res     : in signed(7 downto 0);
      alu_x       : out signed(7 downto 0);
      alu_y       : out signed(7 downto 0));
end datapath;

architecture dpath of datapath is

    component reg8 is
    port (reg_in : in bus8;
          ld, clk : in std_logic;
          reg_out : out bus8);
    end component;

    component shifter is
    port (clk, ld, shl, shr, rol_flag, ror_flag, cflag_in : std_logic;
          din : in std_logic_vector(7 downto 0);
          dout : out std_logic_vector(7 downto 0);
          nflag, zflag, cflag: out std_logic);
    end component;

    component count8 is
    port (clk,ld : in std_logic;
          din : in bus8;
          count : out bus8);
    end component;

    component mux16_8 is
    port (clk, sel : in std_logic;
          mux_in0 : in bus8;
          mux_in1 : in bus8;
          mux_out : out bus8);
    end component;

    signal alu_add, alu_or, alu_and, alu_not, alu_sub, alu_passy,
           ctl_shr, ctl_shl, ctl_ror, ctl_rol : std_logic;
    signal ac_ld, pc_inc, pc_ld, mpx_sel, rem_ld, mem_rd, mem_wr, rrdm_ld,
           wrdm_ld, ri_ld, flags_ld: std_logic;
    signal mem_out_bus, mem_in_bus : bus8;
    signal ac_out, rrdm_out, pc_out, mpx_out, ri_out : bus8;
    signal flags : std_logic_vector(4 downto 0);
    signal nflag, zflag, cflag, vflag, bflag : std_logic;

begin
    flags  <= flags_in;
    nflag  <= flags(0);
    zflag  <= flags(1);
    cflag  <= flags(2);
    vflag  <= flags(3);
    bflag  <= flags(4);

    alu_add   <= control_in(0);
    alu_or    <= control_in(1);
    alu_and   <= control_in(2);
    alu_not   <= control_in(3);
    alu_sub   <= control_in(4);
    alu_passy <= control_in(5);
    ctl_shr   <= control_in(6);
    ctl_shl   <= control_in(7);
    ctl_ror   <= control_in(8);
    ctl_rol   <= control_in(9);
    ac_ld     <= control_in(10);
    flags_ld  <= control_in(11);
    pc_inc    <= control_in(12);
    pc_ld     <= control_in(13);
    mpx_sel   <= control_in(14);
    rem_ld    <= control_in(15);
    mem_rd    <= control_in(16);
    mem_wr    <= control_in(17);
    rrdm_ld   <= control_in(18);
    wrdm_ld   <= control_in(19);
    ri_ld     <= control_in(20);

    ri : reg8
    port map (reg_in  => rrdm_out,
              clk     => clock,
              ld      => ri_ld,
              reg_out => ri_out);

    decode : process (clock)
    begin
        case ri_out is
            when NOPCOD => dec_out <= "100000000000000000000000";
            when STACOD => dec_out <= "010000000000000000000000";
            when LDACOD => dec_out <= "001000000000000000000000";
            when ADDCOD => dec_out <= "000100000000000000000000";
            when ORCOD  => dec_out <= "000010000000000000000000";
            when ANDCOD => dec_out <= "000001000000000000000000";
            when NOTCOD => dec_out <= "000000100000000000000000";
            when SUBCOD => dec_out <= "000000010000000000000000";
            when JMPCOD => dec_out <= "000000001000000000000000";
            when JNCOD  => dec_out <= "000000000100000000000000";
            when JPCOD  => dec_out <= "000000000010000000000000";
            when JVCOD  => dec_out <= "000000000001000000000000";
            when JNVCOD => dec_out <= "000000000000100000000000";
            when JZCOD  => dec_out <= "000000000000010000000000";
            when JNZCOD => dec_out <= "000000000000001000000000";
            when JCCOD  => dec_out <= "000000000000000100000000";
            when JNCCOD => dec_out <= "000000000000000010000000";
            when JBCOD  => dec_out <= "000000000000000001000000";
            when JNBCOD => dec_out <= "000000000000000000100000";
            when SHRCOD => dec_out <= "000000000000000000010000";
            when SHLCOD => dec_out <= "000000000000000000001000";
            when RORCOD => dec_out <= "000000000000000000000100";
            when ROLCOD => dec_out <= "000000000000000000000010";
            when HLTCOD => dec_out <= "000000000000000000000001";
            when others => dec_out <= "000000000000000000000000";
        end case;
    end process;

    accum : shifter
    port map (din      => std_logic_vector(alu_res),
              clk      => clock,
              ld       => ac_ld,
              shl      => ctl_shl,
              shr      => ctl_shr,
              rol_flag => ctl_rol,
              ror_flag => ctl_ror,
              dout     => ac_out,
              cflag_in => cflag,
              cflag    => cflag,
              nflag    => nflag,
              zflag    => zflag);
    
    rrdm : reg8
    port map (reg_in  => mem_out_bus,
              clk     => clock,
              ld      => rrdm_ld,
              reg_out => rrdm_out);

    wrdm : reg8
    port map (reg_in  => ac_out,
              clk     => clock,
              ld      => wrdm_ld,
              reg_out => mem_in_bus);

    pc : count8
    port map (din     => rrdm_out,
              clk     => clock,
              ld      => pc_ld,
              count   => pc_out);

    mpx : mux16_8
    port map (clk     => clock,
              sel     => mpx_sel,
              mux_in0 => pc_out,
              mux_in1 => rrdm_out,
              mux_out => mpx_out);

    mem_out    <= mem_out_bus;
    mem_in_bus <= mem_in;
    flags_out  <= flags;
    accum_out  <= ac_out;

end dpath;

library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity alu is
port (x : in signed(7 downto 0);
      y : in signed(7 downto 0);
      alu_opsel : in std_logic_vector(5 downto 0);
      alu_out : out signed(7 downto 0);
      flags_out : out std_logic_vector(4 downto 0));
end alu;

architecture behv of alu is
    signal res : signed(7 downto 0);
    signal flags : std_logic_vector(4 downto 0);
    signal alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub : std_logic;
    signal nflag, zflag, cflag, vflag, bflag : std_logic;
begin
    nflag <= flags(0);
    zflag <= flags(1);
    cflag <= flags(2);
    vflag <= flags(3);
    bflag <= flags(4);
    alu_add <= alu_opsel(0);
    alu_or  <= alu_opsel(1);
    alu_and <= alu_opsel(2);
    alu_not <= alu_opsel(3);
    alu_py  <= alu_opsel(4);
    alu_sub <= alu_opsel(5);
    flags_out <= "00000";
    alu: process (x, y, alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub)
    variable temp_res : signed (8 downto 0); --one more due to carry flag
    begin
        if (alu_py = '1') then res <= y;
        elsif (alu_add = '1') then
            temp_res := ('0' & x) + y;
            if (x(0) = '1' and y(0) = '1' and temp_res(1) = '0') then
                vflag <= '1';
            elsif (x(0) = '0' and y(0) = '0' and temp_res(1) = '1') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
        elsif (alu_or = '1')  then temp_res := x or y;
        elsif (alu_and = '1') then temp_res := x and y;
        elsif (alu_not = '1') then temp_res := not x;
        elsif (alu_sub = '1') then
            temp_res := ('0' & x) + (not y) + 1;
            if (x(0) = '0' and y(0) = '1' and temp_res(1) = '1') then
                vflag <= '1';
            elsif (x(0) = '1' and y(0) = '0' and temp_res(1) = '0') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
        end if;
        if (temp_res = 0) then flags <= "01000";
        elsif (temp_res < 0) then flags <= "10000";
        end if;
        alu_out <= res;
        res <= temp_res(7 downto 0);
        cflag <= temp_res(0);
        bflag <= not temp_res(0);
    end process;
    flags_out <= flags;
end behv;
    

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;


entity ctrl_unit is
port (clk         : in std_logic;
      reset       : in std_logic;
      flags_in    : in std_logic_vector(4 downto 0);
      dec_in      : in instdec_type;
      control_out : out ctlcod_type);
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

    constant t0_cod : ctlcod_type := "000000000000000100000";
    constant t1_cod : ctlcod_type := "000000000000100010000";
    constant t2_cod : ctlcod_type := "000000000000000000001"; 
                                --    012345678901234567890
    constant t3_op  : ctlcod_type := "000000000000000100000"; 
    constant t3_not : ctlcod_type := "000100000011000000000"; 
    constant t3_brp : ctlcod_type := "000000000000100000000"; 
                                --    012345678901234567890
    constant t4_op  : ctlcod_type := "000000000000100010000"; 
    constant t4_br  : ctlcod_type := "000000000000000010000"; 
    constant t5_op  : ctlcod_type := "000000000000001100000"; 
                                --    012345678901234567890
    constant t5_br  : ctlcod_type := "000000000000010000000"; 
    constant t6_op  : ctlcod_type := "000000000000000010000"; 
    constant t6_sta : ctlcod_type := "000000000000000000010"; 
                                --    012345678901234567890
    constant t7_sta : ctlcod_type := "000000000000000001000"; 
    constant t7_lda : ctlcod_type := "000001000011000000000"; 
    constant t7_add : ctlcod_type := "100000000011000000000"; 
                                --    012345678901234567890
    constant t7_or  : ctlcod_type := "010000000011000000000"; 
    constant t7_and : ctlcod_type := "001000000011000000000"; 
    constant t7_sub : ctlcod_type := "000010000011000000000"; 
                                --    012345678901234567890
    constant t7_shr : ctlcod_type := "000000100011000000000"; 
    constant t7_shl : ctlcod_type := "000000010011000000000"; 
    constant t7_ror : ctlcod_type := "001000001011000000000"; 
    constant t7_rol : ctlcod_type := "010000000111000000000"; 

    signal alunop, alusta, alulda, aluadd, aluor, aluand, alunot, alusub, 
           alujmp, alujn, alujp, alujv, alujnv, alujz, alujnz, alujc, alujnc, 
           alujb, alujnb, alushr, alushl, aluror, alurol, aluhlt : std_logic;
    signal opflag, brflag, counter_ld : std_logic;
    signal NS, PS : std_logic_vector(2 downto 0);

begin
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
    decnot <= dec_in(6);
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
                                --    012345678901234567890

    st_count : count3
    port map (clk   => clk,
              ld    => counter_ld,
              din   => PS,
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
                    if (decsta = '1' or declda = '1' or decand = '1' or
                        decor = '1' or decand = '1' or decsub = '1' or
                        decjmp = '1' or decshr = '1' or decshl = '1' or decrol = '1' or decror = '1') then
                        control_out <= t3_op;
                    elsif (decnot = '1') then
                        control_out <= t3_not;
                    elsif ((decjn = '1' and nflag = '1' ) or
                           (decjp = '1' and nflag = '0') or
                           (decjz = '1' and zflag = '1') or
                           (decjnz = '1' and zflag = '0') or
                           (decjc = '1' and cflag = '1') or
                           (decjnc = '1' and cflag = '0') or
                           (decjv = '1' and vflag = '1') or
                           (decjnv = '1' and vflag = '0') or
                           (decjb = '1' and bflag = '1') or
                           (decjnb = '1' and bflag = '0')) then
                        control_out <= t3_op;
                    elsif  (decnop = '1' or dechlt = '1') then
                        NS <= "000";
                    else
                        control_out <= t3_brp;
                        NS <= "000";
                    end if;
                when "100" =>
                    if (decsta = '1' or declda = '1' or decand = '1' or
                        decor = '1' or decand = '1' or decsub = '1' or
                        decshr = '1' or decshl = '1' or decrol = '1' or decror = '1') then
                        control_out <= t4_op;
                    else control_out <= t4_br;
                    end if;
                when "101" =>
                    if (decsta = '1' or declda = '1' or decand = '1' or
                        decor = '1' or decand = '1' or decsub = '1' or
                        decshr = '1' or decshl = '1' or decrol = '1' or decror = '1') then
                        control_out <= t5_op;
                    else
                        control_out <= t5_br;
                        NS <= "000";
                    end if;
                when "110" =>
                    if (decsta = '1') then
                        control_out <= t6_sta;
                    else control_out <= t6_op;
                    end if;
                when "111" =>
                    if (decsta = '1') then
                        control_out <= t7_sta;
                    elsif (declda = '1') then
                        control_out <= t7_lda;
                    elsif (decadd = '1') then
                        control_out <= t7_add;
                    elsif (decor = '1') then
                        control_out <= t7_or;
                    elsif (decand = '1') then
                        control_out <= t7_and;
                    elsif (decsub = '1') then
                        control_out <= t7_sub;
                    elsif (decshl = '1') then
                        control_out <= t7_sub;
                    else control_out <= (others => '0');
                    end if;
                    NS <= "000";
                when others =>
                    control_out <= (others => '0');
            end case;
        end if;
    end process;
end ctrl_unit;
