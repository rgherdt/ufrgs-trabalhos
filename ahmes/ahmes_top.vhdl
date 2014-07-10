library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity ahmes is
port (clock: in std_logic;
      reset: in std_logic);
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
              dec_out    => instdec,
              mem_in     => mem_out,
              mem_out    => mem_in,
              alu_res    => signed(alu_data),
              std_logic_vector(alu_x) => alu_x,
              std_logic_vector(alu_y) => alu_y);
              
end ahmes;

