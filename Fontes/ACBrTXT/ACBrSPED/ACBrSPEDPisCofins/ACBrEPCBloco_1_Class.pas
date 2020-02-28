{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Fernando Amado                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrEPCBloco_1_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEPCBloco_1, ACBrEPCBlocos,
     ACBrEPCBloco_0_Class;

type
  /// TBloco_1 -

  { TBloco_1 }

  TBloco_1 = class(TACBrSPED)
  private
    FRegistro1001: TRegistro1001;      /// BLOCO 1 - Registro1001
    FRegistro1990: TRegistro1990;      /// BLOCO 1 - Registro1990

    FRegistro1010Count: integer;
    FRegistro1011Count: integer;
    FRegistro1020Count: integer;
    FRegistro1050Count: integer;
    FRegistro1100Count: integer;
    FRegistro1101Count: integer;
    FRegistro1102Count: integer;
    FRegistro1200Count: integer;
    FRegistro1210Count: integer;
    FRegistro1220Count: integer;
    FRegistro1300Count: integer;
    FRegistro1500Count: integer;
    FRegistro1501Count: integer;
    FRegistro1502Count: integer;
    FRegistro1600Count: integer;
    FRegistro1610Count: integer;
    FRegistro1620Count: integer;
    FRegistro1700Count: integer;
    FRegistro1800Count: integer;
    FRegistro1809Count: integer;
    FRegistro1900Count: integer;
    FBloco_0: TBloco_0;

    procedure WriteRegistro1010(Reg1001: TRegistro1001);
    procedure WriteRegistro1011(Reg1010: TRegistro1010);
    procedure WriteRegistro1020(Reg1001: TRegistro1001);
    procedure WriteRegistro1050(Reg1001: TRegistro1001);
    procedure WriteRegistro1100(Reg1001: TRegistro1001);
    procedure WriteRegistro1101(Reg1100: TRegistro1100);
    procedure WriteRegistro1102(Reg1101: TRegistro1101);
    procedure WriteRegistro1200(Reg1001: TRegistro1001);
    procedure WriteRegistro1210(Reg1200: TRegistro1200);
    procedure WriteRegistro1220(Reg1200: TRegistro1200);
    procedure WriteRegistro1300(Reg1001: TRegistro1001);
    procedure WriteRegistro1500(Reg1001: TRegistro1001);
    procedure WriteRegistro1501(Reg1500: TRegistro1500);
    procedure WriteRegistro1502(Reg1501: TRegistro1501);
    procedure WriteRegistro1600(Reg1001: TRegistro1001);
    procedure WriteRegistro1610(Reg1600: TRegistro1600);
    procedure WriteRegistro1620(Reg1600: TRegistro1600);
    procedure WriteRegistro1700(Reg1001: TRegistro1001);
    procedure WriteRegistro1800(Reg1001: TRegistro1001);
    procedure WriteRegistro1809(Reg1800: TRegistro1800);
    procedure WriteRegistro1900(Reg1001: TRegistro1001);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create ;          /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function Registro1001New: TRegistro1001;
    function Registro1010New: TRegistro1010;
    function Registro1011New: TRegistro1011;
    function Registro1020New: TRegistro1020;
    function Registro1050New: TRegistro1050;
    function Registro1100New: TRegistro1100;
    function Registro1101New: TRegistro1101;
    function Registro1102New: TRegistro1102;
    function Registro1200New: TRegistro1200;
    function Registro1210New: TRegistro1210;
    function Registro1220New: TRegistro1220;
    function Registro1300New: TRegistro1300;
    function Registro1500New: TRegistro1500;
    function Registro1501New: TRegistro1501;
    function Registro1502New: TRegistro1502;
    function Registro1600New: TRegistro1600;
    function Registro1610New: TRegistro1610;
    function Registro1620New: TRegistro1620;
    function Registro1700New: TRegistro1700;
    function Registro1800New: TRegistro1800;
    function Registro1900New: TRegistro1900;
    function Registro1809New: TRegistro1809;

    procedure WriteRegistro1001;
    procedure WriteRegistro1990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Registro1001: TRegistro1001 read FRegistro1001 write FRegistro1001;
    property Registro1990: TRegistro1990 read FRegistro1990 write FRegistro1990;

    property Registro1010Count: integer read FRegistro1010Count write FRegistro1010Count;
    property Registro1011Count: integer read FRegistro1011Count write FRegistro1011Count;
    property Registro1020Count: integer read FRegistro1020Count write FRegistro1020Count;
    property Registro1050Count: integer read FRegistro1050Count write FRegistro1050Count;
    property Registro1100Count: integer read FRegistro1100Count write FRegistro1100Count;
    property Registro1101Count: integer read FRegistro1101Count write FRegistro1101Count;
    property Registro1102Count: integer read FRegistro1102Count write FRegistro1102Count;
    property Registro1200Count: integer read FRegistro1200Count write FRegistro1200Count;
    property Registro1210Count: integer read FRegistro1210Count write FRegistro1210Count;
    property Registro1220Count: integer read FRegistro1220Count write FRegistro1220Count;
    property Registro1300Count: integer read FRegistro1300Count write FRegistro1300Count;
    property Registro1500Count: integer read FRegistro1500Count write FRegistro1500Count;
    property Registro1501Count: integer read FRegistro1501Count write FRegistro1501Count;
    property Registro1502Count: integer read FRegistro1502Count write FRegistro1502Count;
    property Registro1600Count: integer read FRegistro1600Count write FRegistro1600Count;
    property Registro1610Count: integer read FRegistro1610Count write FRegistro1610Count;
    property Registro1620Count: integer read FRegistro1620Count write FRegistro1620Count;
    property Registro1700Count: integer read FRegistro1700Count write FRegistro1700Count;
    property Registro1800Count: integer read FRegistro1800Count write FRegistro1800Count;
    property Registro1809Count: integer read FRegistro1809Count write FRegistro1809Count;
    property Registro1900Count: integer read FRegistro1900Count write FRegistro1900Count;
  end;

implementation

{ TBloco_1 }

constructor TBloco_1.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_1.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_1.CriaRegistros;
begin
  FRegistro1001 := TRegistro1001.Create;
  FRegistro1990 := TRegistro1990.Create;

  FRegistro1010Count:= 0;
  FRegistro1011Count:= 0;
  FRegistro1020Count:= 0;
  FRegistro1050Count:= 0;
  FRegistro1100Count:= 0;
  FRegistro1101Count:= 0;
  FRegistro1102Count:= 0;
  FRegistro1200Count:= 0;
  FRegistro1210Count:= 0;
  FRegistro1220Count:= 0;
  FRegistro1300Count:= 0;
  FRegistro1500Count:= 0;
  FRegistro1501Count:= 0;
  FRegistro1502Count:= 0;
  FRegistro1600Count:= 0;
  FRegistro1610Count:= 0;
  FRegistro1620Count:= 0;
  FRegistro1700Count:= 0;
  FRegistro1800Count:= 0;
  FRegistro1809Count:= 0;
  FRegistro1900Count:= 0;

  FRegistro1990.QTD_LIN_1 := 0;
end;

procedure TBloco_1.LiberaRegistros;
begin
  FRegistro1001.Free;
  FRegistro1990.Free;
end;

procedure TBloco_1.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_1.Registro1001New: TRegistro1001;
begin
   Result := FRegistro1001;
end;

function TBloco_1.Registro1010New: TRegistro1010;
begin
   Result := FRegistro1001.Registro1010.New;
end;

function TBloco_1.Registro1011New: TRegistro1011;
var
  U1010Count: integer;
begin
  U1010Count := FRegistro1001.Registro1010.Count -1;
  Result := FRegistro1001.Registro1010.Items[U1010Count].Registro1011.New;
end;

function TBloco_1.Registro1020New: TRegistro1020;
begin
   Result := FRegistro1001.Registro1020.New;
end;

function TBloco_1.Registro1050New: TRegistro1050;
begin
   Result := FRegistro1001.Registro1050.New;
end;

function TBloco_1.Registro1100New: TRegistro1100;
begin
   Result := FRegistro1001.Registro1100.New;
end;

function TBloco_1.Registro1101New: TRegistro1101;
var
U100Count: integer;
begin
   U100Count := FRegistro1001.Registro1100.Count -1;
   //
   Result := FRegistro1001.Registro1100.Items[U100Count].Registro1101.New;
end;

function TBloco_1.Registro1102New: TRegistro1102;
var
U100Count: integer;
U101Count: integer;
begin
   U100Count := FRegistro1001.Registro1100.Count -1;
   U101Count := FRegistro1001.Registro1100.Items[U100Count].Registro1101.Count -1;
   //
   Result := FRegistro1001.Registro1100.Items[U100Count].Registro1101.Items[U101Count].Registro1102;
end;

function TBloco_1.Registro1200New: TRegistro1200;
begin
   Result := FRegistro1001.Registro1200.New;
end;

function TBloco_1.Registro1210New: TRegistro1210;
var
U200Count: integer;
begin
   U200Count := FRegistro1001.Registro1200.Count -1;
   //
   Result := FRegistro1001.Registro1200.Items[U200Count].Registro1210.New;
end;

function TBloco_1.Registro1220New: TRegistro1220;
var
U200Count: integer;
begin
   U200Count := FRegistro1001.Registro1200.Count -1;
   //
   Result := FRegistro1001.Registro1200.Items[U200Count].Registro1220.New;
end;

function TBloco_1.Registro1300New: TRegistro1300;
begin
   Result := FRegistro1001.Registro1300.New;
end;

function TBloco_1.Registro1500New: TRegistro1500;
begin
   Result := FRegistro1001.Registro1500.New;
end;

function TBloco_1.Registro1501New: TRegistro1501;
var
U500Count: integer;
begin
   U500Count := FRegistro1001.Registro1500.Count -1;
   //
   Result := FRegistro1001.Registro1500.Items[U500Count].Registro1501.New;
end;

function TBloco_1.Registro1502New: TRegistro1502;
var
U500Count: integer;
U501Count: integer;
begin
   U500Count := FRegistro1001.Registro1500.Count -1;
   U501Count := FRegistro1001.Registro1500.Items[U500Count].Registro1501.Count -1;
   //
   Result := FRegistro1001.Registro1500.Items[U500Count].Registro1501.Items[U501Count].Registro1502;
end;

function TBloco_1.Registro1600New: TRegistro1600;
begin
   Result := FRegistro1001.Registro1600.New;
end;

function TBloco_1.Registro1610New: TRegistro1610;
var
U600Count: integer;
begin
   U600Count := FRegistro1001.Registro1600.Count -1;
   //
   Result := FRegistro1001.Registro1600.Items[U600Count].Registro1610.New;
end;

function TBloco_1.Registro1620New: TRegistro1620;
var
U600Count: integer;
begin
   U600Count := FRegistro1001.Registro1600.Count -1;
   //
   Result := FRegistro1001.Registro1600.Items[U600Count].Registro1620.New;
end;

function TBloco_1.Registro1700New: TRegistro1700;
begin
   Result := FRegistro1001.Registro1700.New;
end;

function TBloco_1.Registro1800New: TRegistro1800;
begin
   Result := FRegistro1001.Registro1800.New;
end;

function TBloco_1.Registro1809New: TRegistro1809;
var
U800Count: integer;
begin
   U800Count := FRegistro1001.Registro1800.Count -1;
   //
   Result := FRegistro1001.Registro1800.Items[U800Count].Registro1809.New;
end;

procedure TBloco_1.WriteRegistro1001 ;
begin
  if Assigned(FRegistro1001) then
  begin
     with FRegistro1001 do
     begin
        Add( LFill( '1001' ) +
             LFill( Integer(IND_MOV), 0 ) ) ;

        if IND_MOV = imComDados then
        begin
          WriteRegistro1010(FRegistro1001);
          WriteRegistro1020(FRegistro1001);
          WriteRegistro1050(FRegistro1001);
          WriteRegistro1100(FRegistro1001);
          WriteRegistro1200(FRegistro1001);
          WriteRegistro1300(FRegistro1001);
          WriteRegistro1500(FRegistro1001);
          WriteRegistro1600(FRegistro1001);
          WriteRegistro1700(FRegistro1001);
          WriteRegistro1800(FRegistro1001);
          WriteRegistro1900(FRegistro1001);
        end;
     end;
     Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
  end;
end;

procedure TBloco_1.WriteRegistro1010(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1010) then
  begin
     for intFor := 0 to Reg1001.Registro1010.Count - 1 do
     begin
        with Reg1001.Registro1010.Items[intFor] do
        begin
          Add( LFill('1010') +
               LFill( NUM_PROC ) +
               LFill( ID_SEC_JUD ) +
               LFill( ID_VARA ) +
               LFill( IND_NAT_ACAO, 2) + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( DESC_DEC_JUD ) +
               LFill( DT_SENT_JUD ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1011( Reg1001.Registro1010.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1010Count := FRegistro1010Count + Reg1001.Registro1010.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1011(Reg1010: TRegistro1010);
var
  intFor: Integer;
begin
  if Assigned(Reg1010.Registro1011) then
    begin
       for intFor := 0 to Reg1010.Registro1011.Count - 1 do
       begin
          with Reg1010.Registro1011.Items[intFor] do
          begin
             Add(
             {01} LFill('1011')               +
             {02} LFill(REG_REF)              +
             {03} LFill(CHAVE_DOC)            +
             {04} LFill(COD_PART)             +
             {05} LFill(COD_ITEM)             +
             {06} LFill(DT_OPER)              +
             {07} LFill(VL_OPER          ,0, 2) +
             {08} LFill(CST_PIS            , 2) +
             {09} VDFill(VL_BC_PIS         , 4) +
             {10} VDFill(ALIQ_PIS          , 4) +
             {11} VDFill(VL_PIS            , 2) +
             {12} LFill(CST_COFINS         , 2) +
             {13} VDFill(VL_BC_COFINS      , 4) +
             {14} VDFill(ALIQ_COFINS       , 4) +
             {15} VDFill(VL_COFINS         , 2) +
             {16} LFill(CST_PIS_SUSP       , 2) +
             {17} VDFill(VL_BC_PIS_SUSP    , 4) +
             {18} VDFill(ALIQ_PIS_SUSP     , 4) +
             {19} VDFill(VL_PIS_SUSP       , 2) +
             {20} LFill(CST_COFINS_SUSP    , 2) +
             {21} VDFill(VL_BC_COFINS_SUSP , 4) +
             {22} VDFill(ALIQ_COFINS_SUSP  , 4) +
             {23} VDFill(VL_COFINS_SUSP    , 2) +
             {24} LFill(COD_CTA            , 0) +
             {25} LFill(COD_CCUS           , 0) +
             {26} LFill(DESC_DOC_OPER      , 0)
               ) ;
          end;
          ///
          Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
       end;
       /// Variavél para armazenar a quantidade de registro do tipo.
       FRegistro1011Count := FRegistro1011Count + Reg1010.Registro1011.Count;
    end;
end;

procedure TBloco_1.WriteRegistro1020(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1020) then
  begin
     for intFor := 0 to Reg1001.Registro1020.Count - 1 do
     begin
        with Reg1001.Registro1020.Items[intFor] do
        begin

          Add( LFill('1020') +
               LFill( NUM_PROC ) +
               LFill( IND_NAT_ACAO, 2 ) + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( DT_DEC_ADM ) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1020Count := FRegistro1020Count + Reg1001.Registro1020.Count;
  end;
end;


procedure TBloco_1.WriteRegistro1050(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1050) then
  begin
     for intFor := 0 to Reg1001.Registro1050.Count - 1 do
     begin
        with Reg1001.Registro1050.Items[intFor] do
        begin
          Add( LFill('1050') +
               LFill( DT_REF ) +
               LFill( TabCodAjBaseCalcToStr(IND_AJ_BC) ) +
               LFill( CNPJ, 14 ) +
               LFill( VL_AJ_TOT,0,2 ) +
               LFill( VL_AJ_CST01,0,2 ) +
               LFill( VL_AJ_CST02,0,2 ) +
               LFill( VL_AJ_CST03,0,2 ) +
               LFill( VL_AJ_CST04,0,2 ) +
               LFill( VL_AJ_CST05,0,2 ) +
               LFill( VL_AJ_CST06,0,2 ) +
               LFill( VL_AJ_CST07,0,2 ) +
               LFill( VL_AJ_CST08,0,2 ) +
               LFill( VL_AJ_CST09,0,2 ) +
               LFill( VL_AJ_CST49,0,2 ) +
               LFill( VL_AJ_CST99,0,2 ) +
               LFill( IndicadorApropAjusteToStr(IND_APROP) ) +
               LFill( NUM_REC ) +
               LFill( INFO_COMPL ) );
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1050Count := FRegistro1050Count + Reg1001.Registro1050.Count;
  end;
end;


procedure TBloco_1.WriteRegistro1100(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1100) then
  begin
     for intFor := 0 to Reg1001.Registro1100.Count - 1 do
     begin
        with Reg1001.Registro1100.Items[intFor] do
        begin
          Add( LFill('1100') +
               LFill( PER_APU_CRED, 6) +
               LFill( ORIG_CRED, 2) + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( CNPJ_SUC ) +
               LFill( COD_CRED, 3) + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( VL_CRED_APU,0,2 ) +
               LFill( VL_CRED_EXT_APU,0,2 ) +
               LFill( VL_TOT_CRED_APU,0,2 ) +
               LFill( VL_CRED_DESC_PA_ANT,0,2 ) +
               LFill( VL_CRED_PER_PA_ANT,0,2 ) +
               LFill( VL_CRED_DCOMP_PA_ANT,0,2 ) +
               LFill( SD_CRED_DISP_EFD,0,2 ) +
               LFill( VL_CRED_DESC_EFD,0,2 ) +
               LFill( VL_CRED_PER_EFD,0,2 ) +
               LFill( VL_CRED_DCOMP_EFD,0,2 ) +
               LFill( VL_CRED_TRANS,0,2 ) +
               LFill( VL_CRED_OUT,0,2 ) +
               LFill( SLD_CRED_FIM, 0,2 ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1101( Reg1001.Registro1100.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1100Count := FRegistro1100Count + Reg1001.Registro1100.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1101(Reg1100: TRegistro1100) ;
var
intFor: integer;
begin
  if Assigned(Reg1100.Registro1101) then
  begin
     for intFor := 0 to Reg1100.Registro1101.Count - 1 do
     begin
        with Reg1100.Registro1101.Items[intFor] do
        begin
          Add( LFill('1101') +
               LFill( COD_PART ) +
               LFill( COD_ITEM ) +
               LFill( COD_MOD ) +   //Verificar criação da tabela no ACBrEPCBlocos
               LFill( SER ) +       //Verificar criação da tabela no ACBrEPCBlocos
               LFill( SUB_SER ) +
               LFill( NUM_DOC, 0 ) +
               LFill( DT_OPER ) +
               LFill( CHV_NFE ) +
               LFill( VL_OPER,0,2 ) +
               LFill( CFOP,4 ) +
               LFill( NAT_BC_CRED ) + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( IND_ORIG_CRED,0 ) +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( CST_PIS, 2 ) +
               DFill( VL_BC_PIS,3 ) +
               DFill( ALIQ_PIS,4 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( COD_CTA ) +
               LFill( COD_CCUS ) +
               LFill( DESC_COMPL ) +
               LFill( PER_ESCRIT,6 ) +
               LFill( CNPJ ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1102( Reg1100.Registro1101.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1101Count := FRegistro1101Count + Reg1100.Registro1101.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1102(Reg1101: TRegistro1101) ;
begin
  if Assigned(Reg1101.Registro1102) then
  	if (Reg1101.CST_PIS in [53,54,55,56,63,64,65,66]) then
  	begin
      with Reg1101.Registro1102 do
      begin
      	Add( LFill('1102') +
            VDFill( VL_CRED_PIS_TRIB_MI,2 ) +
            VDFill( VL_CRED_PIS_NT_MI,2 ) + //Verificar criação da tabela no ACBrEPCBlocos
            VDFill( VL_CRED_PIS_EXP,2 ) ) ;
      end;
     ///
      Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro1102Count := FRegistro1102Count + 1;
  	end;
end;

procedure TBloco_1.WriteRegistro1200(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1200) then
  begin
     for intFor := 0 to Reg1001.Registro1200.Count - 1 do
     begin
        with Reg1001.Registro1200.Items[intFor] do
        begin
          Add( LFill('1200') +
               LFill( PER_APUR_ANT, 6 )       +
               LFill( NAT_CONT_REC )          +
               LFill( VL_CONT_APUR,0,2 )      +
               LFill( VL_CRED_PIS_DESC,0,2 )  +
               LFill( VL_CONT_DEV,0,2 )       +
               LFill( VL_OUT_DED,0,2 )        +
               LFill( VL_CONT_EXT,0,2 )       +
               LFill( VL_MUL,0,2 )            +
               LFill( VL_JUR,0,2 )            +
               LFill( DT_RECOL ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1210( Reg1001.Registro1200.Items[intFor] );
        WriteRegistro1220( Reg1001.Registro1200.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1200Count := FRegistro1200Count + Reg1001.Registro1200.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1210(Reg1200: TRegistro1200) ;
var
intFor: integer;
begin
  if Assigned(Reg1200.Registro1210) then
  begin
     for intFor := 0 to Reg1200.Registro1210.Count - 1 do
     begin
        with Reg1200.Registro1210.Items[intFor] do
        begin
          Add( LFill('1210') +
               LFill( CNPJ ) +
               LFill( CstPisToStr(CST_PIS) ) +
               LFill( COD_PART ) +
               LFill( DT_OPER ) +
               LFill( VL_OPER,0,2 ) +
               LFill( VL_BC_PIS,0,2 ) +
               LFill( ALIQ_PIS,0,2 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( COD_CTA ) +
               LFill( DESC_COMPL ) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1210Count := FRegistro1210Count + Reg1200.Registro1210.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1220(Reg1200: TRegistro1200) ;
var
intFor: integer;
begin
  if Assigned(Reg1200.Registro1220) then
  begin
     for intFor := 0 to Reg1200.Registro1220.Count - 1 do
     begin
        with Reg1200.Registro1220.Items[intFor] do
        begin
          Add( LFill('1220') +
               LFill( PER_APU_CRED, 6 ) +
               LFill( ORIG_CRED )       +   //Verificar criação da tabela no ACBrEPCBlocos
               LFill( COD_CRED )        +   //Verificar criação da tabela no ACBrEPCBlocos
               LFill( VL_CRED,0,2 ) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1220Count := FRegistro1220Count + Reg1200.Registro1220.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1300(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1300) then
  begin
     for intFor := 0 to Reg1001.Registro1300.Count - 1 do
     begin
        with Reg1001.Registro1300.Items[intFor] do
        begin
          Add( LFill('1300') +
               LFill( IND_NAT_RET, 2 )    +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( PR_REC_RET, 6 )     +
               LFill( VL_RET_APU,0,2 )    +
               LFill( VL_RET_DED,0,2 )    +
               LFill( VL_RET_PER,0,2 )    +
               LFill( VL_RET_DCOMP,0,2 )  +
               LFill( SLD_RET,0,2 ) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1300Count := FRegistro1300Count + Reg1001.Registro1300.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1500(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1500) then
  begin
     for intFor := 0 to Reg1001.Registro1500.Count - 1 do
     begin
        with Reg1001.Registro1500.Items[intFor] do
        begin
          Add( LFill('1500') +
               LFill( PER_APU_CRED, 6 ) +
               LFill( ORIG_CRED, 2 ) +          //Verificar criação da tabela no ACBrEPCBlocos
               LFill( CNPJ_SUC ) +           //Verificar criação da tabela no ACBrEPCBlocos
               LFill( COD_CRED, 3 ) +
               LFill( VL_CRED_APU,0,2 ) +
               LFill( VL_CRED_EXT_APU,0,2 ) +
               LFill( VL_TOT_CRED_APU,0,2 ) +
               LFill( VL_CRED_DESC_PA_ANT,0,2 ) +
               LFill( VL_CRED_PER_PA_ANT,0,2 ) +
               LFill( VL_CRED_DCOMP_PA_ANT,0,2 ) +
               LFill( SD_CRED_DISP_EFD,0,2 ) +
               LFill( VL_CRED_DESC_EFD,0,2 ) +
               LFill( VL_CRED_PER_EFD,0,2 ) +
               LFill( VL_CRED_DCOMP_EFD,0,2 ) +
               LFill( VL_CRED_TRANS,0,2 ) +
               LFill( VL_CRED_OUT,0,2 ) +
               LFill( SLD_CRED_FIM,0,2 ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1501( Reg1001.Registro1500.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1500Count := FRegistro1500Count + Reg1001.Registro1500.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1501(Reg1500: TRegistro1500) ;
var
intFor: integer;
begin
  if Assigned(Reg1500.Registro1501) then
  begin
     for intFor := 0 to Reg1500.Registro1501.Count - 1 do
     begin
        with Reg1500.Registro1501.Items[intFor] do
        begin
          Add( LFill('1501') +
               LFill( COD_PART ) +
               LFill( COD_ITEM ) +
               LFill( COD_MOD ) +   //Verificar criação da tabela no ACBrEPCBlocos
               LFill( SER ) +       //Verificar criação da tabela no ACBrEPCBlocos
               LFill( SUB_SER ) +
               LFill( NUM_DOC,0 ) +
               LFill( DT_OPER ) +
               LFill( CHV_NFE ) +
               LFill( VL_OPER,0,2 ) +
               LFill( CFOP,4 ) +
               LFill( NAT_BC_CRED ) + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( IND_ORIG_CRED,0 ) +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( CST_COFINS,2 ) +
               DFill( VL_BC_COFINS,3 ) +
               DFill( ALIQ_COFINS,4 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( COD_CTA ) +
               LFill( COD_CCUS ) +
               LFill( DESC_COMPL ) +
               LFill( PER_ESCRIT,6 ) +
               LFill( CNPJ ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1502( Reg1500.Registro1501.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1501Count := FRegistro1501Count + Reg1500.Registro1501.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1502(Reg1501: TRegistro1501) ;
begin
  if Assigned(Reg1501.Registro1502) then
	if (Reg1501.CST_COFINS in [53,54,55,56,63,64,65,66]) then
	begin
	 with Reg1501.Registro1502 do
	 begin
	   Add( LFill('1502') +
	        VDFill( VL_CRED_COFINS_TRIB_MI,2 ) +
	        VDFill( VL_CRED_COFINS_NT_MI,2 ) + //Verificar criação da tabela no ACBrEPCBlocos
	        VDFill( VL_CRED_COFINS_EXP,2 ) ) ;
	 end;
	 ///
	 Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
	 /// Variavél para armazenar a quantidade de registro do tipo.
	 FRegistro1502Count := FRegistro1502Count + 1;
	end;
end;

procedure TBloco_1.WriteRegistro1600(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1600) then
  begin
     for intFor := 0 to Reg1001.Registro1600.Count - 1 do
     begin
        with Reg1001.Registro1600.Items[intFor] do
        begin
          Add( LFill('1600') +
               LFill( PER_APUR_ANT, 6)          +
               LFill( NAT_CONT_REC )            +
               LFill( VL_CONT_APUR,0,2 )        +
               LFill( VL_CRED_COFINS_DESC,0,2 ) +
               LFill( VL_CONT_DEV,0,2 )         +
               LFill( VL_OUT_DED,0,2 )          +
               LFill( VL_CONT_EXT,0,2 )         +
               LFill( VL_MUL,0,2 )              +
               LFill( VL_JUR,0,2 )              +
               LFill( DT_RECOL ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1610( Reg1001.Registro1600.Items[intFor] );
        WriteRegistro1620( Reg1001.Registro1600.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1600Count := FRegistro1600Count + Reg1001.Registro1600.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1610(Reg1600: TRegistro1600) ;
var
intFor: integer;
begin
  if Assigned(Reg1600.Registro1610) then
  begin
     for intFor := 0 to Reg1600.Registro1610.Count - 1 do
     begin
        with Reg1600.Registro1610.Items[intFor] do
        begin
          Add( LFill('1610') +
               LFill( CNPJ ) +
               LFill( CstCofinsToStr(CST_COFINS) ) +
               LFill( COD_PART ) +
               LFill( DT_OPER ) +
               LFill( VL_OPER,0,2 ) +
               DFill( VL_BC_COFINS, 3 ) +
               DFill( ALIQ_COFINS, 4 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( COD_CTA ) +
               LFill( DESC_COMPL ) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1610Count := FRegistro1610Count + Reg1600.Registro1610.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1620(Reg1600: TRegistro1600) ;
var
intFor: integer;
begin
  if Assigned(Reg1600.Registro1620) then
  begin
     for intFor := 0 to Reg1600.Registro1620.Count - 1 do
     begin
        with Reg1600.Registro1620.Items[intFor] do
        begin
          Add( LFill('1620') +
               LFill( PER_APU_CRED, 6 ) +
               LFill( ORIG_CRED, 2 )    +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( COD_CRED )        +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( VL_CRED,0,2 ) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1620Count := FRegistro1620Count + Reg1600.Registro1620.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1700(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1700) then
  begin
     for intFor := 0 to Reg1001.Registro1700.Count - 1 do
     begin
        with Reg1001.Registro1700.Items[intFor] do
        begin
          Add( LFill('1700') +
               LFill( IND_NAT_RET, 2 )    +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( PR_REC_RET, 6 )     +
               LFill( VL_RET_APU,0,2 )    +
               LFill( VL_RET_DED,0,2 )    +
               LFill( VL_RET_PER,0,2 )    +
               LFill( VL_RET_DCOMP,0,2 )  +
               LFill( SLD_RET,0,2 ) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1700Count := FRegistro1700Count + Reg1001.Registro1700.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1800(Reg1001: TRegistro1001) ;
var
intFor: integer;
begin
  if Assigned(Reg1001.Registro1800) then
  begin
     for intFor := 0 to Reg1001.Registro1800.Count - 1 do
     begin
        with Reg1001.Registro1800.Items[intFor] do
        begin
          Add( LFill('1800') +
               LFill( INC_IMOB ) +
               LFill( REC_RECEB_RET,0,2 ) +
               LFill( REC_FIN_RET,0,2 ) +
               LFill( BC_RET,0,2 ) +
               LFill( ALIQ_RET,0,2 ) +
               LFill( VL_REC_UNI,0,2 ) +
               LFill( DT_REC_UNI ) +
               LFill( COD_REC ) ) ;
        end;
        // Registros FILHOS
        WriteRegistro1809( Reg1001.Registro1800.Items[intFor] );
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1800Count := FRegistro1800Count + Reg1001.Registro1800.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1809(Reg1800: TRegistro1800) ;
var
intFor: integer;
strIND_PROC : String;
begin
  if Assigned(Reg1800.Registro1809) then
  begin
     for intFor := 0 to Reg1800.Registro1809.Count - 1 do
     begin
        with Reg1800.Registro1809.Items[intFor] do
        begin
           case IND_PROC of
             opJusticaFederal : strIND_PROC := '1';
             opSecexRFB       : strIND_PROC := '3';
             opOutros         : strIND_PROC := '9';
             opNenhum         : strIND_PROC := '';
           end;
           Add( LFill('1809') +
                LFill(NUM_PROC) +
                LFill(strIND_PROC) ) ;
        end;
        ///
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1809Count := FRegistro1809Count + Reg1800.Registro1809.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1990 ;
begin
  if Assigned(Registro1990) then
  begin
     with Registro1990 do
     begin
       QTD_LIN_1 := QTD_LIN_1 + 1;
       ///
       Add( LFill('1990') +
            LFill(QTD_LIN_1,0) );
     end;
  end;
end;

function TBloco_1.Registro1900New: TRegistro1900;
begin
  Result := FRegistro1001.Registro1900.New;
end;

procedure TBloco_1.WriteRegistro1900(Reg1001: TRegistro1001);
var
  intFor:Integer;
begin
  if Assigned(Reg1001.Registro1900) then
  begin
     for intFor := 0 to Reg1001.Registro1900.Count - 1 do
     begin
        with Reg1001.Registro1900.Items[intFor] do
        begin
          //if Length(CNPJ) > 0 then Check(funChecaCNPJ(CNPJ), '(0-1900) %s-%s, o CNPJ "%s" digitado é inválido!', [CNPJ]);
          Add( LFill( '1900'     )     +
               LFill( CNPJ      )      +
               LFill( COD_MOD   )      +
               LFill( SER       )      +
               LFill( SUB_SER    )     +
               LFill( CodSitFToStr(COD_SIT) )      +
               LFill( VL_TOT_REC,0,2 )             +
               LFill( QUANT_DOC,0 )                +
               LFill( CstPisToStr(CST_PIS) )       +
               LFill( CstCofinsToStr(CST_COFINS) ) +
               LFill( CFOP, 4, True )  +
               LFill( INF_COMPL )      +
               LFill( COD_CTA ) )      ;

        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1900Count := FRegistro1900Count + Reg1001.Registro1900.Count;
  end;
end;

end.
