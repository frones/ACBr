{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou                                 }
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

{$I ACBr.inc}

unit ACBrSEF2_BlocoE_1;

interface

Uses
  SysUtils, Classes,
  ACBrSEF2_BlocoE, ACBrSEF2Conversao;

type

  TBloco_E = class(TACBrSEFIIEDOC)
  private
    FRegistroE001: TRegistroSEFE001;
    FRegistroE990: TRegistroSEFE990;

    FRegistroE020Count: Integer;
    FRegistroE025Count: Integer;
    FRegistroE050Count: Integer;
    FRegistroE055Count: Integer;
    FRegistroE060Count: Integer;
    FRegistroE065Count: Integer;
    FRegistroE080Count: Integer;
    FRegistroE085Count: Integer;
    FRegistroE100Count: Integer;
    FRegistroE105Count: Integer;
    FRegistroE120Count: Integer; 
    FRegistroE300Count: Integer;
    FRegistroE305Count: Integer;
    FRegistroE310Count: Integer;
    FRegistroE330Count: Integer;
    FRegistroE340Count: Integer;
    FRegistroE350Count: Integer;
    FRegistroE360Count: Integer;

    FRegistroE500Count: Integer;
    FRegistroE520Count: Integer;
    FRegistroE525Count: Integer;
    FRegistroE540Count: Integer;
    FRegistroE550Count: Integer;
    FRegistroE560Count: Integer;

    //procedure WriteRegistroE003(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE020(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE025(RegE020: TRegistroSEFE020);
    procedure WriteRegistroE050(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE055(RegE050: TRegistroSEFE050);
    procedure WriteRegistroE060(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE065(RegE060: TRegistroSEFE060);
    procedure WriteRegistroE080(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE085(RegE080: TRegistroSEFE080);
    procedure WriteRegistroE100(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE105(RegE100: TRegistroSEFE100);
    procedure WriteRegistroE120(RegE001: TRegistroSEFE001);

    procedure WriteRegistroE300(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE305(RegE300: TRegistroSEFE300);
    procedure WriteRegistroE310(RegE300: TRegistroSEFE300);
    procedure WriteRegistroE330(RegE300: TRegistroSEFE300);
    procedure WriteRegistroE340(RegE300: TRegistroSEFE300);
    procedure WriteRegistroE350(RegE300: TRegistroSEFE300);
    procedure WriteRegistroE360(RegE300: TRegistroSEFE300);

    procedure WriteRegistroE500(RegE001: TRegistroSEFE001);
    procedure WriteRegistroE520(RegE500: TRegistroSEFE500);
    procedure WriteRegistroE525(RegE500: TRegistroSEFE500);
    procedure WriteRegistroE540(RegE500: TRegistroSEFE500);
    procedure WriteRegistroE550(RegE500: TRegistroSEFE500);
    procedure WriteRegistroE560(RegE500: TRegistroSEFE500);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros;

    function RegistroE001New : TRegistroSEFE001;
    function RegistroE003New : TRegistroSEFE003;
    function RegistroE020New : TRegistroSEFE020;
    function RegistroE025New : TRegistroSEFE025;
    function RegistroE050New : TRegistroSEFE050;
    function RegistroE055New : TRegistroSEFE055;
    function RegistroE060New : TRegistroSEFE060;
    function RegistroE065New : TRegistroSEFE065;
    function RegistroE080New : TRegistroSEFE080;
    function RegistroE085New : TRegistroSEFE085;
    function RegistroE100New : TRegistroSEFE100;
    function RegistroE105New : TRegistroSEFE105;
    function RegistroE120New : TRegistroSEFE120;
    function RegistroE300New : TRegistroSEFE300;
    function RegistroE305New : TRegistroSEFE305;
    function RegistroE310New : TRegistroSEFE310;
    function RegistroE330New : TRegistroSEFE330;
    function RegistroE340New : TRegistroSEFE340;
    function RegistroE350New : TRegistroSEFE350;
    function RegistroE360New : TRegistroSEFE360;

    function RegistroE500New : TRegistroSEFE500;
    function RegistroE520New : TRegistroSEFE520;
    function RegistroE525New : TRegistroSEFE525;
    function RegistroE540New : TRegistroSEFE540;
    function RegistroE550New : TRegistroSEFE550;
    function RegistroE560New : TRegistroSEFE560;

    procedure WriteRegistroE001;
    procedure WriteRegistroE990;


    property RegistroE001: TRegistroSEFE001 read FRegistroE001 write FRegistroE001;
    property RegistroE990: TRegistroSEFE990 read FRegistroE990 write FRegistroE990;

    property RegistroE020Count: Integer read FRegistroE020Count write FRegistroE020Count;
    property RegistroE025Count: Integer read FRegistroE025Count write FRegistroE025Count;
    property RegistroE050Count: Integer read FRegistroE050Count write FRegistroE050Count;
    property RegistroE055Count: Integer read FRegistroE055Count write FRegistroE055Count;
    property RegistroE060Count: Integer read FRegistroE060Count write FRegistroE060Count;
    property RegistroE065Count: Integer read FRegistroE065Count write FRegistroE065Count;
    property RegistroE080Count: Integer read FRegistroE080Count write FRegistroE080Count;
    property RegistroE085Count: Integer read FRegistroE085Count write FRegistroE085Count;
    property RegistroE100Count: Integer read FRegistroE100Count write FRegistroE100Count;
    property RegistroE105Count: Integer read FRegistroE105Count write FRegistroE105Count;
    property RegistroE120Count: Integer read FRegistroE120Count write FRegistroE120Count;
    property RegistroE300Count: Integer read FRegistroE300Count write FRegistroE300Count;
    property RegistroE305Count: Integer read FRegistroE305Count write FRegistroE305Count;
    property RegistroE310Count: Integer read FRegistroE310Count write FRegistroE310Count;
    property RegistroE330Count: Integer read FRegistroE330Count write FRegistroE330Count;
    property RegistroE340Count: Integer read FRegistroE340Count write FRegistroE340Count;
    property RegistroE350Count: Integer read FRegistroE350Count write FRegistroE350Count;
    property RegistroE360Count: Integer read FRegistroE360Count write FRegistroE360Count;

    property RegistroE500Count: Integer read FRegistroE500Count write FRegistroE500Count;
    property RegistroE520Count: Integer read FRegistroE520Count write FRegistroE520Count;
    property RegistroE525Count: Integer read FRegistroE525Count write FRegistroE525Count;
    property RegistroE540Count: Integer read FRegistroE540Count write FRegistroE540Count;
    property RegistroE550Count: Integer read FRegistroE550Count write FRegistroE550Count;
    property RegistroE560Count: Integer read FRegistroE560Count write FRegistroE560Count;
  end;


implementation

function IntToStrNull(AInteger : Integer) : string;
begin
   if AInteger = 0 then
     Result := ''
   else
     Result := IntToStr(AInteger);
end;

function FloatToStrBull(AFloat : Currency) : string;
begin
  if AFloat = 0 then
    Result := ''
  else
    Result := Format('%0.2f',[AFloat]);
end;

{ TBlocoE }

constructor TBloco_E.Create;
begin
   inherited ;
   CriaRegistros;
end;

destructor TBloco_E.Destroy;
begin
   LiberaRegistros;
   inherited;
end;

procedure TBloco_E.WriteRegistroE001;
begin
   if Assigned(FRegistroE001) then
   begin
      with FRegistroE001 do
      begin
         Add( LFill( 'E001' ) +
              LFill( IndContDocumentoToStr(IND_DAD), 1));

         WriteRegistroE020(FRegistroE001);
         WriteRegistroE050(FRegistroE001);
         WriteRegistroE060(FRegistroE001);
         WriteRegistroE080(FRegistroE001);
         WriteRegistroE100(FRegistroE001);
         WriteRegistroE120(FRegistroE001); 
         WriteRegistroE300(FRegistroE001);
         WriteRegistroE500(FRegistroE001);
      end;

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;
end;

{
procedure TBloco_E.WriteRegistroE003(RegE001: TRegistroSEFE001);
begin
  Exit;
end;
 }
 
procedure TBloco_E.WriteRegistroE020(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE020: TRegistroSEFE020;
begin
   for intFor := 0 to RegE001.RegistroE020.Count - 1 do
   begin
      RegE020 := TRegistroSEFE020(RegE001.RegistroE020.Items[intFor]);
      with RegE020 do
      begin
          Add( LFill('E020')                                     +
               LFill(IndOperToStr(IND_OPER))                     +
               LFill(IndEmissaoToStr(IND_EMIT))                  +
               LFill(COD_PART)                                   +
               LFill(ModDocumentoToStr(COD_MOD))                 +
               LFill(CodSituacaoToStr(COD_SIT), 2)               +
               LFill(SER)                                        +
               LFill(NUM_DOC,0)                                  +
               LFill(CHV_NFE)                                    +
               LFill(DT_EMIS)                                    +
               LFill(DT_DOC)                                     +
               LFill(COD_NAT)                                    +
               LFill(COP)                                        +
               LFill(NUM_LCTO,0)                                 +
               LFill(IndPagamentoToStr(IND_PGTO))                +
               LFill(VL_CONT,2)                                  +
               LFill(VL_OP_ISS,2)                                +
               LFill(VL_BC_ICMS,2)                               +
               LFill(VL_ICMS,2)                                  +
               LFill(VL_ICMS_ST,2, 2,True)                         +
               LFill(VL_ST_E,2, 2,True)                            +
               LFill(VL_ST_S,2, 2,True)                            +
               LFill(VL_AT,2, 2, True)                              +
               LFill(VL_ISNT_ICMS,2)                             +
               LFill(VL_OUT_ICMS,2)                              +
               LFill(VL_BC_IPI,2, 2, True)                       +
               LFill(VL_IPI,2, 2, True)                          +
               LFill(VL_ISNT_IPI,2, 2, True)                     +
               LFill(VL_OUT_IPI,2, 2, True)                      +
               LFill(COD_INF_OBS));
      end;

      WriteRegistroE025(RegE020);

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE020Count := FRegistroE020Count + RegE001.RegistroE020.Count;
end;

procedure TBloco_E.WriteRegistroE025(RegE020: TRegistroSEFE020);
var
  intFor : Integer;
  RegE025: TRegistroSEFE025;
begin
   if Assigned(RegE020.RegistroE025) then
   begin
      for intFor := 0 to RegE020.RegistroE025.Count - 1 do
      begin
         RegE025 := TRegistroSEFE025(RegE020.RegistroE025.Items[intFor]);
         with RegE025 do
         begin
            Add( LFill('E025')                   +
                 LFill(VL_CONT_P, 2)             +
                 LFill(VL_OP_ISS_P, 2)           +
                 LFill(CFOP)                     +
                 LFill(VL_BC_ICMS_P, 2)          +
                 LFill(ALIQ_ICMS, 2, 2, True)    +
                 LFill(VL_ICMS_P, 2)             +
                 LFill(VL_BC_ST_P, 2, 2,True)      +
                 LFill(VL_ICMS_ST_P, 2, 2,True)    +
                 LFill(VL_ISNT_ICMS_P, 2, 2,True)  +
                 LFill(VL_OUT_ICMS_P, 2, 2,True)   +
                 LFill(VL_BC_IPI_P, 2, 2,True)     +
                 LFill(VL_IPI_P, 2, 2,True)        +
                 LFill(VL_ISNT_IPI_P, 2, 2,True)   +
                 LFill(VL_OUT_IPI_P, 2, 2, True) +
                 LFill(IND_PETR, 1)              +
                 LFill(IND_IMUN, 1, True));
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE025Count := FRegistroE025Count + RegE020.RegistroE025.Count;
   end;
end;

procedure TBloco_E.WriteRegistroE050(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE050: TRegistroSEFE050;
begin
   for intFor := 0 to RegE001.RegistroE050.Count - 1 do
   begin
      RegE050 := TRegistroSEFE050(RegE001.RegistroE050.Items[intFor]);
      with RegE050 do
      begin
         Add( LFill('E050')                                     +
              LFill(ModDocumentoToStr(COD_MOD))                 +
              LFill(QTD_CANC,0)                                 +
              LFill(SER)                                        +
              LFill(SUB,2)                                      +
              LFill(NUM_DOC_INI,0)                              +
              LFill(NUM_DOC_FIN,0)                              +
              LFill(DT_DOC)                                     +
              LFill(COP)                                        +
              LFill(NUM_LCTO,0)                                 +
              LFill(VL_CONT,2)                                  +
              LFill(VL_BC_ICMS,2)                               +
              LFill(VL_ICMS,2)                                  +
              LFill(VL_ISNT_ICMS,2)                             +
              LFill(VL_OUT_ICMS,2)                              +
              LFill(COD_INF_OBS));
      end;

      WriteRegistroE055(RegE050);

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE050Count := FRegistroE050Count + RegE001.RegistroE050.Count;

end;

procedure TBloco_E.WriteRegistroE055(RegE050: TRegistroSEFE050);
var
  intFor : Integer;
  RegE055: TRegistroSEFE055;
begin
   if Assigned(RegE050.RegistroE055) then
   begin
      for intFor := 0 to RegE050.RegistroE055.Count - 1 do
      begin
         RegE055:= TRegistroSEFE055(RegE050.RegistroE055.Items[intFor]);
         with RegE055  do
         begin
            Add( LFill('E055')                   +
                 LFill(VL_CONT_P, 2)             +
                 LFill(CFOP)                     +
                 LFill(VL_BC_ICMS_P, 2)          +
                 LFill(ALIQ_ICMS, 2, 2, True)    +
                 LFill(VL_ICMS_P, 2)             +
                 LFill(VL_ISNT_ICMS_P, 2)        +
                 LFill(VL_OUT_ICMS_P, 2)         +
                 LFill(IND_IMUN));
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE055Count := FRegistroE055Count + RegE050.RegistroE055.Count;
   end;

end;

procedure TBloco_E.WriteRegistroE060(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE060: TRegistroSEFE060;
begin
   for intFor := 0 to RegE001.RegistroE060.Count - 1 do
   begin
      RegE060 := TRegistroSEFE060(RegE001.RegistroE060.Items[intFor]);
      with RegE060 do
      begin
         Add( LFill('E060')                                     +
              LFill(ModDocumentoToStr(COD_MOD))                 +
              LFill(ECF_CX,0)                                   +
              LFill(ECF_FAB)                                    +
              LFill(CRO,0)                                      +
              LFill(CRZ,0)                                      +
              LFill(DT_DOC)                                     +
              LFill(NUM_DOC_INI,0)                              +
              LFill(NUM_DOC_FIN,0)                              +
              LFill(GT_INI,2)                                   +
              LFill(GT_FIN,2)                                   +
              LFill(VL_BRT,2)                                   +
              LFill(VL_CANC_ICMS,2)                             +
              LFill(VL_DESC_ICMS,2)                             +
              LFill(VL_ACMO_ICMS,2)                             +
              LFill(VL_OP_ISS,2)                                +
              LFill(VL_LIQ,2)                                   +
              LFill(VL_BC_ICMS,2)                               +
              LFill(VL_ICMS,2)                                  +
              LFill(VL_ISN,2)                                   +
              LFill(VL_NT,2)                                    +
              LFill(VL_ST,2)                                    +
              LFill(COD_INF_OBS));
      end;

      WriteRegistroE065(RegE060);

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE060Count := FRegistroE060Count + RegE001.RegistroE060.Count;
end;

procedure TBloco_E.WriteRegistroE065(RegE060: TRegistroSEFE060);
var
  intFor : Integer;
  RegE065: TRegistroSEFE065;
begin
   if Assigned(RegE060.RegistroE065) then
   begin
      for intFor := 0 to RegE060.RegistroE065.Count - 1 do
      begin
         RegE065:= TRegistroSEFE065(RegE060.RegistroE065.Items[intFor]);
         with RegE065  do
         begin
            Add( LFill('E065')          +
                 LFill(CFOP)            +
                 LFill(VL_BC_ICMS_P, 2) +
                 LFill(ALIQ_ICMS, 2)    +
                 LFill(VL_ICMS_P, 2)    +
                 LFill(IND_IMUN));
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE065Count := FRegistroE065Count + RegE060.RegistroE065.Count;
   end;
end;

procedure TBloco_E.WriteRegistroE080(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE080: TRegistroSEFE080;
begin
   for intFor := 0 to RegE001.RegistroE080.Count - 1 do
   begin
      RegE080 := TRegistroSEFE080(RegE001.RegistroE080.Items[intFor]);
      with RegE080 do
      begin
         Add( LFill('E080')         +
              LFill(IND_TOT, 1)     +
              LFill(ModDocumentoToStr(COD_MOD)) +
              LFill(NUM_MR, 2)      +
              LFill(DT_DOC)         +
              LFill(VL_BRT,2)       +
              LFill(VL_CANC_ICMS,2) +
              LFill(VL_DESC_ICMS,2) +
              LFill(VL_ACMO_ICMS,2) +
              LFill(VL_OP_ISS,2)    +
              LFill(COP)            +
              LFill(NUM_LCTO)       +
              LFill(VL_CONT,2)      +
              LFill(VL_BC_ICMS,2)   +
              LFill(VL_ICMS,2)      +
              LFill(VL_ISNT_ICMS,2) +
              LFill(VL_ST,2)        +
              LFill(IND_OBS));
      end;

      WriteRegistroE085(RegE080);

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE080Count := FRegistroE080Count + RegE001.RegistroE080.Count;
end;

procedure TBloco_E.WriteRegistroE085(RegE080: TRegistroSEFE080);
var
   intFor : Integer;
   RegE085: TRegistroSEFE085;
begin
   if Assigned(RegE080.RegistroE085) then
   begin
      for intFor := 0 to RegE080.RegistroE085.Count - 1 do
      begin
         RegE085:= TRegistroSEFE085(RegE080.RegistroE085.Items[intFor]);
         with RegE085  do
         begin
            Add( LFill('E085')           +
                 LFill(VL_CONT_P,2)      +
                 LFill(VL_OP_ISS_P,2)    +
                 LFill(CFOP,4)           +
                 LFill(VL_BC_ICMS_P,2)   +
                 LFill(ALIQ_ICMS,2)      +
                 LFill(VL_ICMS_P,2)      +
                 LFill(VL_ISNT_ICMS_P,2) +
                 LFill(VL_ST_P,2)        +
                 LFill(IND_IMUN));
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE085Count := FRegistroE085Count + RegE080.RegistroE085.Count;
   end;
end;

procedure TBloco_E.WriteRegistroE100(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE100: TRegistroSEFE100;
begin
   for intFor := 0 to RegE001.RegistroE100.Count - 1 do
   begin
      RegE100 := TRegistroSEFE100(RegE001.RegistroE100.Items[intFor]);
      with RegE100 do
      begin
          Add( LFill('E100')                                     +
               LFill(IndOperToStr(IND_OPER))                     +
               LFill(IndEmissaoToStr(IND_EMIT))                  +
               LFill(COD_PART)                                   +
               LFill(COD_MUN_SERV)                               +
               LFill(ModDocumentoToStr(COD_MOD))                 +
               LFill(CodSituacaoToStr(COD_SIT))                  +
               LFill(QTD_CANC)                                   +               
               LFill(SER)                                        +
               LFill(SUB)                                        +
               LFill(COD_CONS)                                   +               
               LFill(NUM_DOC,0)                                  +
               LFill(QTD_DOC)                                    +               
               LFill(DT_EMIS)                                    +
               LFill(DT_DOC)                                     +
               LFill(COP)                                        +
               LFill(NUM_LCTO,0)                                 +
               LFill(VL_CONT,2)                                  +
               LFill(VL_OP_ISS,2)                                +
               LFill(VL_BC_ICMS,2)                               +
               LFill(VL_ICMS,2)                                  +
               LFill(VL_ICMS_ST,2, 2, True)                      +
               LFill(VL_ISNT_ICMS,2)                             +
               LFill(VL_OUT_ICMS,2)                              +
               LFill(COD_INF_OBS));
      end;

      WriteRegistroE105(RegE100);

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE100Count := FRegistroE100Count + RegE001.RegistroE100.Count;
end;

procedure TBloco_E.WriteRegistroE105(RegE100: TRegistroSEFE100);
var
  intFor : Integer;
  RegE105: TRegistroSEFE105;
begin
   if Assigned(RegE100.RegistroE105) then
   begin
      for intFor := 0 to RegE100.RegistroE105.Count - 1 do
      begin
         RegE105 := TRegistroSEFE105(RegE100.RegistroE105.Items[intFor]);
         with RegE105 do
         begin
            Add( LFill('E105')                   +
                 LFill(VL_CONT_P, 2)             +
                 LFill(VL_OP_ISS_P, 2)           +
                 LFill(CFOP)                     +
                 LFill(VL_BC_ICMS_P, 2)          +
                 LFill(ALIQ_ICMS, 2, 2, True)    +
                 LFill(VL_ICMS_P, 2)             +
                 LFill(VL_ICMS_ST_P, 2)          +
                 LFill(VL_ISNT_ICMS_P, 2)        +
                 LFill(VL_OUT_ICMS_P, 2)         +
                 LFill(IND_PETR, 1));
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE105Count := FRegistroE105Count + RegE100.RegistroE105.Count;
   end;
end;

procedure TBloco_E.WriteRegistroE120(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE120: TRegistroSEFE120;
begin
   for intFor := 0 to RegE001.RegistroE120.Count - 1 do
   begin
      RegE120 := TRegistroSEFE120(RegE001.RegistroE120.Items[intFor]);
      with RegE120 do
      begin
          Add( LFill('E120')                                     +
               LFill(IndOperToStr(IND_OPER))                     +
               LFill(IndEmissaoToStr(IND_EMIT))                  +
               LFill(COD_PART)                                   +
               LFill(COD_MUN_SERV,0)                               +
               LFill(ModDocumentoToStr(COD_MOD))                 +
               LFill(CodSituacaoToStr(COD_SIT),2)                +
               LFill(SER)                                        +
               LFill(SUB,0,true)                                 +
               LFill(NUM_DOC,0)                                  +
               LFill(CHV_CTE)                                    +
               LFill(DT_EMIS)                                    +
               LFill(DT_DOC)                                     +
               LFill(COP)                                        +
               LFill(NUM_LCTO,0)                                 +
               LFill(IndPagamentoToStr(IND_PGTO))                +
               LFill(VL_CONT,2)                                  +
               LFill(CFOP,4)                                    +
               LFill(VL_BC_ICMS,2)                               +
               LFill(AL_ICMS,2,2,true)                           +
               LFill(VL_ICMS,2)                                  +
               LFill(VL_ICMS_ST,2, 2, True)                      +
               LFill(VL_ISNT_ICMS,2)                             +
               LFill(VL_OUT_ICMS,2)                              +
               LFill(COD_INF_OBS));
      end;

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE120Count := FRegistroE120Count + RegE001.RegistroE120.Count;
end;


procedure TBloco_E.WriteRegistroE300(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE300: TRegistroSEFE300;
begin
   for intFor := 0 to RegE001.RegistroE300.Count - 1 do
   begin
      RegE300 := TRegistroSEFE300(RegE001.RegistroE300.Items[intFor]);
      with RegE300 do
      begin
         Add( LFill('E300')         +
              LFill(DT_INI)         +
              LFill(DT_FIN));
      end;

      WriteRegistroE305(RegE300);
      WriteRegistroE310(RegE300);
      WriteRegistroE330(RegE300);
      WriteRegistroE340(RegE300);
      WriteRegistroE350(RegE300);
      WriteRegistroE360(RegE300);

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE300Count := FRegistroE300Count + RegE001.RegistroE300.Count;
end;

procedure TBloco_E.WriteRegistroE305(RegE300: TRegistroSEFE300);
var
   intFor : Integer;
   RegE305: TRegistroSEFE305;
begin
   if Assigned(RegE300.RegistroE305) then
   begin
      for intFor := 0 to RegE300.RegistroE305.Count - 1 do
      begin
         RegE305:= TRegistroSEFE305(RegE300.RegistroE305.Items[intFor]);
         with RegE305  do
         begin
            Add( LFill('E305')         +
                 LFill(IND_MRO,1)      +
                 LFill(IND_OPER,1)     +
                 LFill(DT_DOC)         +
                 LFill(COP)            +
                 LFill(NUM_LCTO)       +
                 LFill(QTD_LCTO,1)     +
                 LFill(VL_CONT, 2)     +
                 LFill(VL_OP_ISS, 2)   +
                 LFill(VL_BC_ICMS, 2)  +
                 LFill(VL_ICMS, 2)     +
                 LFill(VL_ICMS_ST, 2)  +
                 LFill(VL_ST_ENT, 2)   +
                 LFill(VL_ST_FNT, 2)   +
                 LFill(VL_ST_UF, 2)    +
                 LFill(VL_ST_OE, 2)    +
                 LFill(VL_AT, 2)       +
                 LFill(VL_ISNT_ICMS, 2)+
                 LFill(VL_OUT_ICMS, 2) +
                 LFill(VL_BC_IPI, 2)   +
                 LFill(VL_IPI, 2)      +
                 LFill(VL_ISNT_IPI, 2) +
                 LFill(VL_OUT_IPI, 2) );
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE305Count := FRegistroE305Count + RegE300.RegistroE305.Count;
   end;
end;

procedure TBloco_E.WriteRegistroE310(RegE300: TRegistroSEFE300);
var
 intFor : Integer;
 RegE310: TRegistroSEFE310;
begin
   if Assigned(RegE300.RegistroE310) then
   begin
      for intFor := 0 to RegE300.RegistroE310.Count - 1 do
        begin
         RegE310:= TRegistroSEFE310(RegE300.RegistroE310.Items[intFor]);
         with RegE310  do
            begin
              Add( LFill('E310')          +
                   LFill(VL_CONT, 2)      +
                   LFill(VL_OP_ISS, 2)    +
                   LFill(CFOP, 4)         +
                   LFill(VL_BC_ICMS, 2)   +
                   LFill(VL_ICMS, 2)      +
                   LFill(VL_ICMS_ST, 2)   +
                   Lfill(VL_ISNT_ICMS, 2) +
                   LFill(VL_OUT_ICMS, 2)  +
                   LFill(IND_IMUN, 1));
             end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
         end;
     /// Variav?para armazenar a quantidade de registro do tipo.
     FRegistroE310Count := FRegistroE310Count + RegE300.RegistroE310.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE330(RegE300: TRegistroSEFE300);
var
 intFor : Integer;
 RegE330: TRegistroSEFE330;
begin
   if Assigned(RegE300.RegistroE330) then
   begin
      for intFor := 0 to RegE300.RegistroE330.Count - 1 do
        begin
         RegE330:= TRegistroSEFE330(RegE300.RegistroE330.Items[intFor]);
         with RegE330  do
            begin
              Add( LFill('E330')          +
                   LFill(IND_TOT, 1)      +
                   LFill(VL_CONT, 2)      +
                   LFill(VL_OP_ISS, 2)    +
                   LFill(VL_BC_ICMS, 2)   +
                   LFill(VL_ICMS, 2)      +
                   LFill(VL_ICMS_ST, 2)   +
                   LFill(VL_ST_ENT, 2)    +
                   LFill(VL_ST_FNT, 2)    +
                   LFill(VL_ST_UF, 2)     +
                   LFill(VL_ST_OE, 2)     +
                   LFill(VL_AT, 2)        +
                   LFill(VL_ISNT_ICMS, 2) +
                   LFill(VL_OUT_ICMS, 2));
             end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
         end;
     /// Variav?para armazenar a quantidade de registro do tipo.
     FRegistroE330Count := FRegistroE330Count + RegE300.RegistroE330.Count;
  end;
end;


procedure TBloco_E.WriteRegistroE340(RegE300: TRegistroSEFE300);
var
 intFor : Integer;
 RegE340: TRegistroSEFE340;
begin
  if Assigned(RegE300.RegistroE340) then
  begin
  for intFor := 0 to RegE300.RegistroE340.Count - 1 do
    begin
     RegE340:= TRegistroSEFE340(RegE300.RegistroE340.Items[intFor]);
     with RegE340  do
        begin
          Add( LFill('E340')    +
               LFill(VL_01, 2)  +
               LFill(VL_02, 2)  +
               LFill(VL_03, 2)  +
               LFill(VL_04, 2)  +
               LFill(VL_05, 2)  +
               LFill(VL_06, 2)  +
               LFill(VL_07, 2)  +
               LFill(VL_08, 2)  +
               LFill(VL_09, 2)  +
               LFill(VL_10, 2)  +
               LFill(VL_11, 2)  +
               LFill(VL_12, 2)  +
               LFill(VL_13, 2)  +
               LFill(VL_14, 2)  +
               LFill(VL_15, 2)  +
               LFill(VL_16, 2)  +
               LFill(VL_17, 2)  +
               LFill(VL_18, 2)  +
               LFill(VL_19, 2)  +
               LFill(VL_20, 2)  +
               LFill(VL_21, 2)  +
               LFill(VL_22, 2)  +
               LFill(VL_99, 2));
         end;
     RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
     /// Variavel para armazenar a quantidade de registro do tipo.
     FRegistroE340Count := FRegistroE340Count + 1;
    end;
  end;
end;

procedure TBloco_E.WriteRegistroE350(RegE300: TRegistroSEFE300);
var
 intFor : Integer;
 RegE350: TRegistroSEFE350;
begin
   if Assigned(RegE300.RegistroE350) then
   begin
      for intFor := 0 to RegE300.RegistroE350.Count - 1 do
        begin
         RegE350:= TRegistroSEFE350(RegE300.RegistroE350.Items[intFor]);
         with RegE350  do
            begin
              Add( LFill('E350')      +
                   LFill(UF_AJ, 2)    +
                   LFill(COD_AJ, 3)   +
                   LFill(VL_AJ, 2)    +
        		   LFill(NUM_DA)  	  +
        		   LFill(NUM_PROC)    +
        		   LFill(IND_PROC)    +
        		   LFill(DESC_PROC)   +
                   Lfill(COD_INF_OBS) +
                   LFill(IND_AP));
              end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
         end;
     /// Variav?para armazenar a quantidade de registro do tipo.
     FRegistroE350Count := FRegistroE350Count + RegE300.RegistroE350.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE360(RegE300: TRegistroSEFE300);
var
 intFor : Integer;
 RegE360: TRegistroSEFE360;
begin
   if Assigned(RegE300.RegistroE360) then
   begin
      for intFor := 0 to RegE300.RegistroE360.Count - 1 do
        begin
         RegE360:= TRegistroSEFE360(RegE300.RegistroE360.Items[intFor]);
         with RegE360  do
            begin
              Add( LFill('E360')          +
                   LFill(UF_OR, 2) +
                   LFill(COD_OR, 3) +
                   LFill(PER_REF, 6) +
                   LFill(COD_REC) +
                   LFill(VL_ICMS_REC, 2) +
                   LFill(DT_VCTO) +
                   LFill(NUM_PROC) +
                   LFill(IND_PROC) +
                   LFill(DESCR_PROC) +
                   LFill(COD_INF_OBS));
            end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
         end;
     /// Variav?para armazenar a quantidade de registro do tipo.
     FRegistroE360Count := FRegistroE360Count + RegE300.RegistroE360.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE500(RegE001: TRegistroSEFE001);
var
  intFor : Integer;
  RegE500: TRegistroSEFE500;
begin
   for intFor := 0 to RegE001.RegistroE500.Count - 1 do
   begin
      RegE500 := TRegistroSEFE500(RegE001.RegistroE500.Items[intFor]);
      with RegE500 do
      begin
         Add( LFill('E500')         +
              LFill(DT_INI)         +
              LFill(DT_FIN));
      end;

      WriteRegistroE520(RegE500);
      WriteRegistroE525(RegE500);
      WriteRegistroE540(RegE500);
      WriteRegistroE550(RegE500);
      WriteRegistroE560(RegE500);

      RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
   end;

   FRegistroE500Count := FRegistroE500Count + RegE001.RegistroE500.Count;
end;

procedure TBloco_E.WriteRegistroE520(RegE500: TRegistroSEFE500);
var
   intFor : Integer;
   RegE520: TRegistroSEFE520;
begin
   if Assigned(RegE500.RegistroE520) then
   begin
      for intFor := 0 to RegE500.RegistroE520.Count - 1 do
      begin
         RegE520:= TRegistroSEFE520(RegE500.RegistroE520.Items[intFor]);
         with RegE520  do
         begin
            Add( LFill('E520')         +
                 LFill(VL_CONT, 2)     +
                 LFill(CFOP,4)         +
                 LFill(VL_BC_IPI, 2)   +
                 LFill(VL_IPI, 2)      +
                 LFill(VL_ISNT_IPI, 2) +
                 LFill(VL_OUT_IPI, 2)  );
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE520Count := FRegistroE520Count + RegE500.RegistroE520.Count;
   end;
end;

procedure TBloco_E.WriteRegistroE525(RegE500: TRegistroSEFE500);
var
   intFor : Integer;
   RegE525: TRegistroSEFE525;
begin
   if Assigned(RegE500.RegistroE525) then
   begin
      for intFor := 0 to RegE500.RegistroE525.Count - 1 do
      begin
         RegE525:= TRegistroSEFE525(RegE500.RegistroE525.Items[intFor]);
         with RegE525  do
         begin
            Add( LFill('E525')         +
                 LFill(IND_TOT, 1)      +
                 LFill(VL_CONT, 2)      +
                 LFill(VL_BC_IPI, 2)   +
                 LFill(VL_IPI, 2)      +
                 LFill(VL_ISNT_IPI, 2) +
                 LFill(VL_OUT_IPI, 2));
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE525Count := FRegistroE525Count + RegE500.RegistroE525.Count;
   end;
end;
procedure TBloco_E.WriteRegistroE540(RegE500: TRegistroSEFE500);
var
   intFor : Integer;
   RegE540: TRegistroSEFE540;
begin
   if Assigned(RegE500.RegistroE540) then
   begin
      for intFor := 0 to RegE500.RegistroE540.Count - 1 do
      begin
         RegE540:= TRegistroSEFE540(RegE500.RegistroE540.Items[intFor]);
         with RegE540  do
         begin
            Add( LFill('E540')         +
               LFill(VL_01, 2)  +
               LFill(VL_02, 2)  +
               LFill(VL_03, 2)  +
               LFill(VL_04, 2)  +
               LFill(VL_05, 2)  +
               LFill(VL_06, 2)  +
               LFill(VL_07, 2)  +
               LFill(VL_09, 2)  +
               LFill(VL_10, 2)  +
               LFill(VL_11, 2)  +
               LFill(VL_12, 2)  +
               LFill(VL_13, 2)  +
               LFill(VL_08, 2)  +
               LFill(VL_16, 2)  +
               LFill(VL_17, 2)  );
         end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
      end;
      FRegistroE540Count := FRegistroE540Count + RegE500.RegistroE540.Count;
   end;
end;

procedure TBloco_E.WriteRegistroE550(RegE500: TRegistroSEFE500);
var
 intFor : Integer;
 RegE550: TRegistroSEFE550;
begin
   if Assigned(RegE500.RegistroE550) then
   begin
      for intFor := 0 to RegE500.RegistroE550.Count - 1 do
        begin
         RegE550:= TRegistroSEFE550(RegE500.RegistroE550.Items[intFor]);
         with RegE550  do
            begin
              Add( LFill('E550')          +
                   LFill(COD_AJ,3) +
                   LFill(VL_AJ, 2) +
                   LFill(IND_DOC) +
                   LFill(NUM_DOC) +
                   LFill(DESCR_AJ) +
                   LFill(COD_INF_OBS));
            end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
         end;
     /// Variavé¬ para armazenar a quantidade de registro do tipo.
     FRegistroE550Count := FRegistroE550Count + RegE500.RegistroE550.Count;
  end;

end;

procedure TBloco_E.WriteRegistroE560(RegE500: TRegistroSEFE500);
var
 intFor : Integer;
 RegE560: TRegistroSEFE560;
begin
   if Assigned(RegE500.RegistroE560) then
   begin
      for intFor := 0 to RegE500.RegistroE560.Count - 1 do
        begin
         RegE560:= TRegistroSEFE560(RegE500.RegistroE560.Items[intFor]);
         with RegE560  do
            begin
              Add( LFill('E560')          +
                   LFill(COD_OR_IPI) +
                   LFill(PER_REF, 6) +
                   LFill(COD_REC_IPI,4) +
                   LFill(VL_IPI_REC, 2) +
                   LFill(DT_VCTO) +
                   LFill(IND_DOC) +
                   LFill(NUM_DOC) +
                   LFill(DESCR_AJ) +
                   LFill(COD_INF_OBS));
            end;
         RegistroE990.QTD_LIN_E := RegistroE990.QTD_LIN_E + 1;
         end;
     /// Variavé¬ para armazenar a quantidade de registro do tipo.
     FRegistroE560Count := FRegistroE560Count + RegE500.RegistroE560.Count;
  end;
end;

//

procedure TBloco_E.WriteRegistroE990;
var
  strLinha : String;
begin
   //--Before
   strLinha := '';

   if Assigned(RegistroE990) then
   begin
      with RegistroE990 do
      begin
        QTD_LIN_E := QTD_LIN_E + 1;

        strLinha := LFill('E990') +
                    LFill(QTD_LIN_E,0);
        Add(strLinha);
      end;
   end;
end;

procedure TBloco_E.CriaRegistros;
begin
   FRegistroE001 := TRegistroSEFE001.Create;
   FRegistroE990 := TRegistroSEFE990.Create;

   FRegistroE020Count := 0;
   FRegistroE025Count := 0;
   FRegistroE060Count := 0;
   FRegistroE080Count := 0;
   FRegistroE085Count := 0;
   FRegistroE100Count := 0;
   FRegistroE105Count := 0;
   FRegistroE120Count := 0; 
   FRegistroE300Count := 0;
   FRegistroE500Count := 0;
   FRegistroE990.QTD_LIN_E := 0;
end;

procedure TBloco_E.LiberaRegistros;
begin
   FRegistroE001.Free;
   FRegistroE990.Free;
end;

function TBloco_E.RegistroE003New: TRegistroSEFE003;
begin
   Result := FRegistroE001.RegistroE003.New(FRegistroE001);
end;

function TBloco_E.RegistroE020New: TRegistroSEFE020;
begin
   Result := FRegistroE001.RegistroE020.New();
end;

 
function TBloco_E.RegistroE100New: TRegistroSEFE100;
begin
   Result := FRegistroE001.RegistroE100.New();
end;

function TBloco_E.RegistroE105New: TRegistroSEFE105;
var E100: TRegistroSEFE100;
begin
   with FRegistroE001.RegistroE100 do
      E100 := TRegistroSEFE100(Items[AchaUltimoPai('E100', 'E105') ]);
   Result := E100.RegistroE105.New();
end;

function TBloco_E.RegistroE120New: TRegistroSEFE120;
begin
   Result := FRegistroE001.RegistroE120.New(FRegistroE001);
end;

procedure TBloco_E.LimpaRegistros;
begin
   /// Limpa os Registros
   LiberaRegistros;
   Conteudo.Clear;

   /// Recriar os Registros Limpos
   CriaRegistros;
end;

function TBloco_E.RegistroE025New: TRegistroSEFE025;
var
  E020: TRegistroSEFE020;
begin
   with FRegistroE001.RegistroE020 do
      E020 := TRegistroSEFE020(Items[AchaUltimoPai('E020', 'E025') ]);
   Result := E020.RegistroE025.New(E020);
end;

function TBloco_E.RegistroE050New: TRegistroSEFE050;
begin
   Result := FRegistroE001.RegistroE050.New();
end;

function TBloco_E.RegistroE055New: TRegistroSEFE055;
var
  E050: TRegistroSEFE050;
begin
   with FRegistroE001.RegistroE050 do
      E050 := TRegistroSEFE050(Items[ AchaUltimoPai('E050', 'E055') ]);

   Result := E050.RegistroE055.New();
end;

function TBloco_E.RegistroE060New: TRegistroSEFE060;
begin
   Result := FRegistroE001.RegistroE060.New();
end;

function TBloco_E.RegistroE065New: TRegistroSEFE065;
var
  E060: TRegistroSEFE060;
begin
   with FRegistroE001.RegistroE060 do
      E060 := TRegistroSEFE060(Items[ AchaUltimoPai('E060', 'E065') ]);

   Result := E060.RegistroE065.New();
end;

function TBloco_E.RegistroE080New: TRegistroSEFE080;
begin
   Result := FRegistroE001.RegistroE080.New();
end;

function TBloco_E.RegistroE085New: TRegistroSEFE085;
var
  E080: TRegistroSEFE080;
begin
   with FRegistroE001.RegistroE080 do
      E080 := TRegistroSEFE080(Items[ AchaUltimoPai('E080', 'E085') ]);

   Result := E080.RegistroE085.New();
end;

function TBloco_E.RegistroE300New: TRegistroSEFE300;
begin
   Result := FRegistroE001.RegistroE300.New();
end;

function TBloco_E.RegistroE305New: TRegistroSEFE305;
var
  E300: TRegistroSEFE300;
begin
   with FRegistroE001.RegistroE300 do
      E300 := TRegistroSEFE300(Items[ AchaUltimoPai('E300', 'E305') ]);

   Result := E300.RegistroE305.New();
end;

function TBloco_E.RegistroE310New: TRegistroSEFE310;
var
  E300: TRegistroSEFE300;
begin
   with FRegistroE001.RegistroE300 do
      E300 := TRegistroSEFE300(Items[ AchaUltimoPai('E300', 'E310') ]);

   Result := E300.RegistroE310.New();
end;

function TBloco_E.RegistroE330New: TRegistroSEFE330;
var
  E300: TRegistroSEFE300;
begin
   with FRegistroE001.RegistroE300 do
      E300 := TRegistroSEFE300(Items[ AchaUltimoPai('E300', 'E330') ]);

   Result := E300.RegistroE330.New();
end;

function TBloco_E.RegistroE340New: TRegistroSEFE340;
var
  E300: TRegistroSEFE300;
begin
   with FRegistroE001.RegistroE300 do
      E300 := TRegistroSEFE300(Items[ AchaUltimoPai('E300', 'E340') ]);

   Result := E300.RegistroE340.New();
end;

function TBloco_E.RegistroE350New: TRegistroSEFE350;
var
  E300: TRegistroSEFE300;
begin
   with FRegistroE001.RegistroE300 do
      E300 := TRegistroSEFE300(Items[ AchaUltimoPai('E300', 'E350') ]);

   Result := E300.RegistroE350.New();
end;

function TBloco_E.RegistroE360New: TRegistroSEFE360;
var
  E300: TRegistroSEFE300;
begin
   with FRegistroE001.RegistroE300 do
      E300 := TRegistroSEFE300(Items[ AchaUltimoPai('E300', 'E360') ]);

   Result := E300.RegistroE360.New();
end;

function TBloco_E.RegistroE500New: TRegistroSEFE500;
begin
   Result := FRegistroE001.RegistroE500.New();
end;

function TBloco_E.RegistroE520New: TRegistroSEFE520;
var
  E500: TRegistroSEFE500;
begin
   with FRegistroE001.RegistroE500 do
      E500 := TRegistroSEFE500(Items[ AchaUltimoPai('E500', 'E520') ]);

   Result := E500.RegistroE520.New();
end;

function TBloco_E.RegistroE525New: TRegistroSEFE525;
var
  E500: TRegistroSEFE500;
begin
   with FRegistroE001.RegistroE500 do
      E500 := TRegistroSEFE500(Items[ AchaUltimoPai('E500', 'E525') ]);

   Result := E500.RegistroE525.New();
end;

function TBloco_E.RegistroE540New: TRegistroSEFE540;
var
  E500: TRegistroSEFE500;
begin
   with FRegistroE001.RegistroE500 do
      E500 := TRegistroSEFE500(Items[ AchaUltimoPai('E500', 'E540') ]);

   Result := E500.RegistroE540.New();
end;

function TBloco_E.RegistroE550New: TRegistroSEFE550;
var
  E500: TRegistroSEFE500;
begin
   with FRegistroE001.RegistroE500 do
      E500 := TRegistroSEFE500(Items[ AchaUltimoPai('E500', 'E550') ]);

   Result := E500.RegistroE550.New();

end;

function TBloco_E.RegistroE560New: TRegistroSEFE560;
var
  E500: TRegistroSEFE500;
begin
   with FRegistroE001.RegistroE500 do
      E500 := TRegistroSEFE500(Items[ AchaUltimoPai('E500', 'E560') ]);

   Result := E500.RegistroE560.New();
end;


function TBloco_E.RegistroE001New: TRegistroSEFE001;
begin
   Result := FRegistroE001;
end;

end.
