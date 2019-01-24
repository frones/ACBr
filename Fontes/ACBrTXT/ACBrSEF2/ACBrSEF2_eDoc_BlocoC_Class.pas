{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 23/08/2013: Juliana Tamizou
|*  - Distribuição da Primeira Versao
*******************************************************************************}
{$I ACBr.inc}

unit ACBrSEF2_eDoc_BlocoC_Class;

interface

Uses
  SysUtils, Classes,
  ACBrSEF2Conversao, ACBrSEF2_eDoc_BlocoC;

type 

  { TBloco_C }

  TBloco_C = class(TACBrSEFIIEDOC)
  private
    FRegistroC001 : TRegistroSEFC001;
    FRegistroC990 : TRegistroSEFC990;

    FRegistroC020Count: Integer;
    FRegistroC040Count: Integer;
    FRegistroC300Count: Integer;
    FRegistroC310Count: Integer;
    FRegistroC550Count: Integer;
    FRegistroC560Count: Integer;
    FRegistroC600Count: Integer;
    FRegistroC605Count: Integer;
    FRegistroC610Count: Integer;
    FRegistroC615Count: Integer;

    procedure WriteRegistroC020(RegC001: TRegistroSEFC001);
    procedure WriteRegistroC040(RegC020: TRegistroSEFC020);
    procedure WriteRegistroC300(RegC020: TRegistroSEFC020);
    procedure WriteRegistroC310(RegC300: TRegistroSEFC300);
    procedure WriteRegistroC550(RegC001: TRegistroSEFC001);
    procedure WriteRegistroC560(RegC550: TRegistroSEFC550);
    procedure WriteRegistroC600(RegC001: TRegistroSEFC001);
    procedure WriteRegistroC605(RegC600: TRegistroSEFC600);
    procedure WriteRegistroC610(RegC600: TRegistroSEFC600);
    procedure WriteRegistroC615(RegC610: TRegistroSEFC610);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy;override;
    procedure LimpaRegistros;

    function RegistroC001New: TRegistroSEFC001;
    function RegistroC020New: TRegistroSEFC020;
    function RegistroC040New: TRegistroSEFC040;
    function RegistroC300New: TRegistroSEFC300;
    function RegistroC310New: TRegistroSEFC310;
    function RegistroC550New: TRegistroSEFC550;
    function RegistroC560New: TRegistroSEFC560;
    function RegistroC600New: TRegistroSEFC600;
    function RegistroC605New: TRegistroSEFC605;
    function RegistroC610New: TRegistroSEFC610;
    function RegistroC615New: TRegistroSEFC615;
    function RegistroC990New: TRegistroSEFC990;

    procedure WriteRegistroC001;
    procedure WriteRegistroC990;

    property RegistroC001: TRegistroSEFC001 read FRegistroC001 write FRegistroC001;
    property RegistroC990: TRegistroSEFC990 read FRegistroC990 write FRegistroC990;

    property RegistroC020Count: Integer read FRegistroC020Count write FRegistroC020Count;
    property RegistroC040Count: Integer read FRegistroC040Count write FRegistroC040Count;
    property RegistroC300Count: Integer read FRegistroC300Count write FRegistroC300Count;
    property RegistroC310Count: Integer read FRegistroC310Count write FRegistroC310Count;
    property RegistroC550Count: Integer read FRegistroC550Count write FRegistroC550Count;
    property RegistroC560Count: Integer read FRegistroC560Count write FRegistroC560Count;
    property RegistroC600Count: Integer read FRegistroC600Count write FRegistroC600Count;
    property RegistroC605Count: Integer read FRegistroC605Count write FRegistroC605Count;
    property RegistroC610Count: Integer read FRegistroC610Count write FRegistroC610Count;
    property RegistroC615Count: Integer read FRegistroC615Count write FRegistroC615Count;
  end;


implementation

function ConvertCodigoSituacao(CodigoSituacao : TCodigoSituacao) : string;
begin
  Case CodigoSituacao of
    SefcsEmissaonormal           : Result := '00';
    SefcsEmissaocontingencia     : Result := '01';
    SefcsEmissaocontingenciaFS   : Result := '02';
    SefcsEmissaocontingenciaSCAN : Result := '03';
    SefcsEmissaocontingenciaDPEC : Result := '04';
    SefcsEmissaocontingenciaFSDA : Result := '05';
    SefcsEmissaoavulsa           : Result := '10';
    SefcsComplemento             : Result := '20';
    SefcsConsolidavalores        : Result := '25';
    SefcsAutorizadenegada        : Result := '80';
    SefcsNumerainutilizada       : Result := '81';
    SefcsOperacancelada          : Result := '90';
    SefcsNegociodesfeito         : Result := '91';
    SefcsAjusteinformacoes       : Result := '95';
    SefcsSemrepercussaofiscal    : Result := '99';
  end;
end;

function ConvertSEFIIDocFiscalReferenciado(SEFIIDocFiscalReferenciado : TSEFIIDocFiscalReferenciado) : string;
begin
  Case SEFIIDocFiscalReferenciado of
    SrefNF      : Result := '01';
    SrefNFVCCVC : Result := '02';
    SrefCCF     : Result := '2D';
    SrefCBP     : Result := '2E';
    SrefNFPR    : Result := '04';
    SrefNFEE    : Result := '06';
    SrefNFTR    : Result := '07';
    SrefCTRC    : Result := '08';
    SrefCTAQ    : Result := '09';
    SrefCTAR    : Result := '10';
    SrefCTFC    : Result := '11';
    SrefBPR     : Result := '13';
    SrefBPAQ    : Result := '14';
    SrefBPNB    : Result := '15';
    SrefBPF     : Result := '16';
    SrefDT      : Result := '17';
    SrefRMD     : Result := '18';
    SrefOCC     : Result := '20';
    SrefNFSC    : Result := '21';
    SrefNFST    : Result := '22';
    SrefGNRE    : Result := '23';
    SrefACT     : Result := '24';
    SrefMC      : Result := '25';
    SrefCTMC    : Result := '26';
    SrefNFTF    : Result := '27';
    SrefNFGC    : Result := '28';
    SrefNFAC    : Result := '29';
    SrefMV      : Result := '30';
    SrefBRP     : Result := '31';
    SrefNFe     : Result := '55';
    SrefCTe     : Result := '57';
  end;
end;

{ TBlocoC }

constructor TBloco_C.Create;
begin
  inherited Create;
  CriaRegistros;
end;

destructor TBloco_C.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_C.LimpaRegistros;
begin
   /// Limpa os Registros
   LiberaRegistros;
   Conteudo.Clear;

   /// Recriar os Registros Limpos
   CriaRegistros;
end;

function TBloco_C.RegistroC001New: TRegistroSEFC001;
begin
  Result := FRegistroC001;
end;

function TBloco_C.RegistroC990New: TRegistroSEFC990;
begin
  Result := FRegistroC990;
end;

procedure TBloco_C.LiberaRegistros;
begin
  FRegistroC001.Free;
  FRegistroC990.Free;
end;

procedure TBloco_C.CriaRegistros;
begin
  FRegistroC001 := TRegistroSEFC001.Create;
  FRegistroC990 := TRegistroSEFC990.Create;

  FRegistroC020Count := 0;
  FRegistroC040Count := 0;
  FRegistroC300Count := 0;
  FRegistroC550Count := 0;
  FRegistroC560Count := 0;
  FRegistroC600Count := 0;
  FRegistroC605Count := 0;
  FRegistroC610Count := 0;
  FRegistroC615Count := 0;
  FRegistroC990.QTD_LIN_C := 0;
  
end;

procedure TBloco_C.WriteRegistroC001;
begin
   if Assigned(FRegistroC001) then
   begin
      with FRegistroC001 do
      begin
         Add( LFILL('C001') +
              LFill(Integer(IND_DAD),0) );

         if IND_DAD = icContConteudo then
         begin
            WriteRegistroC020(FRegistroC001);
            WriteRegistroC550(FRegistroC001);
            WriteRegistroC600(FRegistroC001);
         end;

         FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
      end;
   end;
end;

procedure TBloco_C.WriteRegistroC020(RegC001: TRegistroSEFC001);
var
 intFor  : Integer;
 RegC020 : TRegistroSEFC020;
 wCOD_MOD: String;
 wCOD_SIT: String;
begin
   for intFor := 0 to RegC001.RegistrosC020.Count - 1 do
   begin
      RegC020 := TRegistroSEFC020(RegC001.RegistrosC020.Itens[intFor]);
      with RegC020 do
      begin
         wCOD_MOD :=  ConvertSEFIIDocFiscalReferenciado(COD_MOD);
         wCOD_SIT :=  ConvertCodigoSituacao(COD_SIT);
         Add( LFill('C020')                      +
              LFill(Integer(IND_OPER), 1)        +
              LFill(Integer(IND_EMIT), 0)        +
              LFill(COD_PART)                    +
              LFill(wCOD_MOD)                    +
              LFill(wCOD_SIT)                    +
              LFill(SER,3)                       +
              LFill(NUM_DOC, 0)                  +
              LFill(CHV_NFE)                     +
              LFill(DT_EMIS)                     +
              LFill(DT_DOC)                      +
              LFill(COD_NAT)                     +
              LFill(IndPagamentoToStr(IND_PGTO),0) +
              LFill(VL_DOC, 2)                   +
              LFill(VL_DESC, 2)                  +
              LFill(VL_ACMO, 2)                  +
              LFill(VL_MERC, 2)                  +
              LFill(VL_FRT, 2)                   +
              LFill(VL_SEG, 2)                   +
              LFill(VL_OUT_DA, 2)                +
              LFill(VL_OP_ISS, 2, 2, True)       +
              LFill(VL_BC_ICMS, 2)               +
              LFill(VL_ICMS, 2)                  +
              LFill(VL_BC_ST, 2, 2, True)        +
              LFill(VL_ICMS_ST, 2, 2,True)       +
              LFill(VL_AT, 2, 2, True)           +
              LFill(VL_IPI, 2, 2, True)          +
              LFill(COD_INF_OBS) );
      end;

      WriteRegistroC040(RegC020);
      WriteRegistroC300(RegC020);

      FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
   end;

   FRegistroC020Count := FRegistroC020Count + RegC001.RegistrosC020.Count;
end;

procedure TBloco_C.WriteRegistroC040(RegC020: TRegistroSEFC020);
begin
   if Assigned(RegC020.RegistrosC040) then
   begin
     with RegC020.RegistrosC040 do
     begin
        Add( LFill('C040')          +
             LFill(COD_MUN_SERV)    +
             DFill(VL_BC_ISS,2, True)     +
             DFill(VL_ISS,2, True)        +
             DFill(VL_BC_RT_ISS,2, True)  +
             DFill(VL_RT_ISS, 2, True));
     end;
     FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
   end;
   FRegistroC040Count := FRegistroC040Count + 1;
end;

procedure TBloco_C.WriteRegistroC600(RegC001: TRegistroSEFC001);
var
  intFor: Integer;
  wCOD_MOD : string;
  RegC600: TRegistroSEFC600;
begin
   for intFor := 0 to RegC001.RegistrosC600.Count - 1 do
   begin
      RegC600 := TRegistroSEFC600(RegC001.RegistrosC600.Items[intFor]);
      with RegC600 do
      begin
         wCOD_MOD :=  ConvertSEFIIDocFiscalReferenciado(COD_MOD);
         Add( LFill('C600')                         +
              LFill(CPF_CONS)                       +
              LFill(CNPJ_CONS)                      +
              LFill(wCOD_MOD)                       +
              LFill(ConvertCodigoSituacao(COD_SIT)) +
              LFill(ECF_CX, 0)                      +
              LFill(ECF_FAB)                        +
              LFill(CRO, 0)                         +
              LFill(CRZ, 0)                         +
              LFill(NUM_DOC, 0)                     +
              LFill(DT_DOC)                         +
              LFill(COP)                            +
              LFill(VL_DOC, 2)                      +
              LFill(VL_CANC_ICMS, 2)                +
              LFill(VL_DESC_ICMS, 2)                +
              LFill(VL_ACMO_ICMS, 2)                +
              LFill(VL_OP_ISS, 2, 2, True)          +
              LFill(VL_BC_ICMS, 2)                  +
              LFill(VL_ICMS, 2)                     +
              LFill(VL_ISN, 2)                      +
              LFill(VL_NT, 2)                       +
              LFill(VL_ST, 2) );
      end;

      WriteRegistroC605(RegC600);
      WriteRegistroC610(RegC600);

      FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
   end;

   FRegistroC600Count := FRegistroC600Count + RegC001.RegistrosC600.Count;
end;

procedure TBloco_C.WriteRegistroC605(RegC600: TRegistroSEFC600);
begin
   if Assigned(RegC600.RegistroC605) then
   begin
      with RegC600.RegistroC605 do
      begin
         Add( LFill('C605')          +
              LFill(VL_CANC_ISS, 2)  +
              LFill(VL_DESC_ISS, 2)  +
              LFill(VL_ACMO_ISS, 2)  +
              LFill(VL_BC_ISS  , 2)  +
              LFill(VL_ISS     , 2)  +
              LFill(VL_ISN_ISS , 2)  +
              LFill(VL_NT_ISS  , 2)
             );

         FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
      end;

      FRegistroC605Count := FRegistroC605Count + 1;
   end;
end;

procedure TBloco_C.WriteRegistroC990;
begin
   if Assigned(FRegistroC990) then
   begin
      with FRegistroC990 do
      begin
         QTD_LIN_C := QTD_LIN_C + 1;

         Add( LFill('C990') +
              LFill(QTD_LIN_C, 0) );
      end;
   end;
end;

procedure TBloco_C.WriteRegistroC300(RegC020: TRegistroSEFC020);
var
  intFor: Integer;
  C300: TRegistroSEFC300;
begin
   if Assigned(RegC020.RegistrosC300) then
   begin
      for intFor := 0 to RegC020.RegistrosC300.Count - 1 do
      begin
         C300 := TRegistroSEFC300(RegC020.RegistrosC300.Items[intFor]);
         with C300 do
         begin
            Add( LFill('C300')                +
                 LFill(NUM_ITEM, 0)           +
                 LFill(COD_ITEM)              +
                 LFill(UNID)                  +
                 DFill(VL_UNIT,3)             +
                 DFill(QTD, 3)                +
                 LFill(VL_DESC_I, 2)          +
                 LFill(VL_ACMO_I, 2)          +
                 LFill(VL_ITEM, 2)            +
                 LFill(COD_NCM)               +
                 LFill(CST, 0)                +
                 LFill(CFOP, 0)               +
                 LFill(VL_BC_ICMS_I, 2)       +
                 LFill(ALIQ_ICMS, 2, 2, True) +
                 LFill(VL_ICMS_I, 2)          +
                 LFill(VL_BC_ST_I, 2)         +
                 LFill(ALIQ_ST, 2, 2, True)   +
                 LFill(VL_ICMS_ST_I, 2)       +
                 LFill(VL_BC_IPI, 2, 2, True) +
                 LFill(ALIQ_IPI, 2, 2, True)  +
                 LFill(VL_IPI_I, 2, 2, True));
         end;
         WriteRegistroC310(C300);

         FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
      end;
   end;
   FRegistroC300Count := FRegistroC300Count + RegC020.RegistrosC300.Count;
end;

procedure TBloco_C.WriteRegistroC310(RegC300: TRegistroSEFC300);
begin
   if Assigned(RegC300.RegistroC310) and (trim(RegC300.RegistroC310.CTISS) <> '') then
   begin
      with RegC300.RegistroC310 do
      begin
         Add( LFill('C310')          +
              LFill(CTISS)           +
              LFill(VL_BC_ISS_I, 2)  +
              LFill(ALIQ_ISS   , 2)  +
              LFill(VL_ISS_I   , 2)
             );

         FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
      end;

      FRegistroC310Count := FRegistroC310Count + 1;
   end;
end;

procedure TBloco_C.WriteRegistroC550(RegC001: TRegistroSEFC001);
var
 intFor  : Integer;
 RegC550 : TRegistroSEFC550;
 wCOD_MOD: String;
 wCOD_SIT: String;
begin
   for intFor := 0 to RegC001.RegistrosC550.Count - 1 do
   begin
      RegC550 := TRegistroSEFC550(RegC001.RegistrosC550.Itens[intFor]);
      with RegC550 do
      begin
         wCOD_MOD := ConvertSEFIIDocFiscalReferenciado(COD_MOD);
         wCOD_SIT := ConvertCodigoSituacao(COD_SIT);
         Add( LFill('C550')        +
              LFill(CPF_CONS)      +
              LFill(CNPJ_CONS)     +
              LFill(wCOD_MOD)      +
              LFill(wCOD_SIT)      +
              LFill(SERIE)         +
              LFill(SUBSERIE)      +
              LFill(NUM_DOC, 01)   +
              LFill(DT_DOC)        +
              LFill(COP)           +
              LFill(VL_DOC, 2)     +
              LFill(VL_DESC, 2)    +
              LFill(VL_ACMO, 2)    +
              LFill(VL_MERC, 2)    +
              LFill(VL_BC_ICMS, 2) +
              LFill(VL_ICMS, 2)    +
              LFill(COD_INF_OBS) );
      end;

      WriteRegistroC560(RegC550);

      FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
   end;

   FRegistroC550Count := FRegistroC550Count + RegC001.RegistrosC550.Count;

end;

procedure TBloco_C.WriteRegistroC560(RegC550: TRegistroSEFC550);
var
  intFor: Integer;
  C560: TRegistroSEFC560;
begin
   if Assigned(RegC550.RegistroC560) then
   begin
      for intFor := 0 to RegC550.RegistroC560.Count - 1 do
      begin
         C560 := TRegistroSEFC560(RegC550.RegistroC560.Items[intFor]);
         with C560 do
         begin
            Add( LFill('C560')                +
                 LFill(NUM_ITEM, 0)           +
                 LFill(COD_ITEM)              +
                 LFill(UNID)                  +
                 DFill(VL_UNIT,3)             +
                 DFill(QTD, 3)                +
                 LFill(VL_DESC_I, 2)          +
                 LFill(VL_ACMO_I, 2)          +
                 LFill(VL_ITEM, 2)            +
                 LFill(CST, 0)                +
                 LFill(CFOP, 0)               +
                 LFill(VL_BC_ICMS_I, 2)       +
                 LFill(ALIQ_ICMS, 2, 2, True) +
                 LFill(VL_ICMS_I, 2));
         end;
         FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
      end;
   end;
   FRegistroC560Count := FRegistroC560Count + RegC550.RegistroC560.Count;
end;

procedure TBloco_C.WriteRegistroC610(RegC600: TRegistroSEFC600);
var
  intFor: Integer;
  C610: TRegistroSEFC610;
begin
   if Assigned(RegC600.RegistroC610) then
   begin
      for intFor := 0 to RegC600.RegistroC610.Count - 1 do
      begin
         C610 := TRegistroSEFC610(RegC600.RegistroC610.Items[intFor]);
         with C610 do
         begin
            Add( LFill('C610') +
                 LFill(NUM_ITEM, 0) +
                 LFill(COD_ITEM) +
                 LFill(UNID) +
                 LFill(VL_UNIT, 6) +
                 LFill(QTD, 6) +
                 LFill(VL_DESC_I, 2) +
                 LFill(VL_ACMO_I, 2) +
                 LFill(VL_ITEM, 2) +
                 LFill(CST) +
                 LFill(CFOP, 4, true) +
                 LFill(VL_BC_ICMS_I, 2) +
                 LFill(ALIQ_ICMS, 2) +
                 LFill(VL_ICMS_I, 2) +
                 LFill(VL_ISN_I, 2) +
                 LFill(VL_NT_I, 2) +
                 LFill(VL_ST_I, 2) );
         end;

         WriteRegistroC615(C610);

         FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
      end;
   end;

   FRegistroC610Count := FRegistroC610Count + RegC600.RegistroC610.Count;
end;

procedure TBloco_C.WriteRegistroC615(RegC610: TRegistroSEFC610);
begin
   if Assigned(RegC610.RegistroC615) then
   begin
      with RegC610.RegistroC615 do
      begin
         Add( LFill('C615')          +
              LFill(VL_BC_ISS_I , 2)  +
              LFill(ALIQ_ISS    , 2)  +
              LFill(VL_ISS_I    , 2)  +
              LFill(VL_ISN_ISS_I, 2)  +
              LFill(VL_NT_ISS_I , 2)
             );

         FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
      end;

      FRegistroC615Count := FRegistroC615Count + 1;
   end;
end;

function TBloco_C.RegistroC020New: TRegistroSEFC020;
begin
   Result := FRegistroC001.RegistrosC020.New();
end;

function TBloco_C.RegistroC040New: TRegistroSEFC040;
var
  C020: TRegistroSEFC020;
begin
   with FRegistroC001.RegistrosC020 do
     C020 := TRegistroSEFC020(Items[ AchaUltimoPai('C020', 'C040') ]);

   C020.RegistrosC040 := TRegistroSEFC040.Create;
   Result := C020.RegistrosC040;
end;

function TBloco_C.RegistroC600New: TRegistroSEFC600;
begin
   Result := FRegistroC001.RegistrosC600.New();
end;

function TBloco_C.RegistroC605New: TRegistroSEFC605;
var
  C600: TRegistroSEFC600;
begin
   with FRegistroC001.RegistrosC600 do
     C600 := TRegistroSEFC600(Items[ AchaUltimoPai('C600', 'C605') ]);

   C600.RegistroC605 := TRegistroSEFC605.Create;
   Result := C600.RegistroC605;
end;

function TBloco_C.RegistroC610New: TRegistroSEFC610;
var
  C600: TRegistroSEFC600;
begin
   with FRegistroC001.RegistrosC600 do
     C600 := TRegistroSEFC600(Items[ AchaUltimoPai('C600', 'C610') ]);

   Result := C600.RegistroC610.New(C600);
end;

function TBloco_C.RegistroC615New: TRegistroSEFC615;
var
  C600: TRegistroSEFC600;
  C610: TRegistroSEFC610;
begin
   with FRegistroC001.RegistrosC600 do
     C600 := TRegistroSEFC600(Items[ AchaUltimoPai('C600', 'C610') ]);

   with C600.RegistroC610 do
     C610 := TRegistroSEFC610(Items[AchaUltimoPai('C610', 'C315')]);

   C610.RegistroC615 := TRegistroSEFC615.Create;
   Result := C610.RegistroC615;
end;

function TBloco_C.RegistroC300New: TRegistroSEFC300;
var
  C020: TRegistroSEFC020;
begin
   with FRegistroC001.RegistrosC020 do
     C020 := TRegistroSEFC020(Items[ AchaUltimoPai('C020', 'C300') ]);

   Result := C020.RegistrosC300.New();
end;

function TBloco_C.RegistroC310New: TRegistroSEFC310;
var
  C020: TRegistroSEFC020;
  C300: TRegistroSEFC300;
begin
   with FRegistroC001.RegistrosC020 do
     C020 := TRegistroSEFC020(Items[ AchaUltimoPai('C020', 'C300') ]);

   with C020.RegistrosC300 do
     C300 := TRegistroSEFC300(Items[AchaUltimoPai('C300', 'C310')]);

   C300.RegistroC310 := TRegistroSEFC310.Create;
   Result := C300.RegistroC310;
end;

function TBloco_C.RegistroC550New: TRegistroSEFC550;
begin
   Result := FRegistroC001.RegistrosC550.New();
end;

function TBloco_C.RegistroC560New: TRegistroSEFC560;
var
  C550: TRegistroSEFC550;
begin
   with FRegistroC001.RegistrosC550 do
     C550 := TRegistroSEFC550(Items[ AchaUltimoPai('C550', 'C560') ]);

   Result := C550.RegistroC560.New(C550);
end;

end.
