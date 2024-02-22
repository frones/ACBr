{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti e Isaque Pinheiro            }
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

{******************************************************************************
|* Historico
|*
|* 11/09/2015 - Ariel Guareschi - Identar no padrao utilizado pela ACBr        
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_0_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_0, ACBrECFBlocos,
  ACBrTXTClass;

type
	{ TBloco_0 }

  TBloco_0 = class(TACBrSPED)
  private
    FRegistro0000: TRegistro0000;
    fRegistro0001: TRegistro0001;
    fRegistro0010: TRegistro0010;
    fRegistro0020: TRegistro0020;
    fRegistro0021: TRegistro0021;
    fRegistro0030: TRegistro0030;
    fRegistro0035: TRegistro0035List;
    fRegistro0930: TRegistro0930List;
    FRegistro0990: TRegistro0990;

    FRegistro0010Count : Integer;
    FRegistro0020Count : Integer;
    FRegistro0021Count : Integer;
    FRegistro0030Count : Integer;
    FRegistro0035Count : Integer;
    FRegistro0930Count : Integer;
    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LimpaRegistros; override;

    function Registro0000New : TRegistro0000;
    function Registro0001New : TRegistro0001;
    function Registro0010New : TRegistro0010;
    function Registro0020New : TRegistro0020;
    function Registro0021New : TRegistro0021;
    function Registro0030New : TRegistro0030;
    function Registro0035New : TRegistro0035;
    function Registro0930New : TRegistro0930;


    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0010;
    procedure WriteRegistro0020;
    procedure WriteRegistro0021;
    procedure WriteRegistro0030;
    procedure WriteRegistro0035(Reg0001: TRegistro0001);
    procedure WriteRegistro0930(Reg0001: TRegistro0001);
    procedure WriteRegistro0990;

    property Registro0000 : TRegistro0000 read FRegistro0000 write FRegistro0000;
    property Registro0001 : TRegistro0001 read fRegistro0001 write fRegistro0001;
    property Registro0010 : TRegistro0010 read fRegistro0010 write fRegistro0010;
    property Registro0020 : TRegistro0020 read fRegistro0020 write fRegistro0020;
    property Registro0021 : TRegistro0021 read fRegistro0021 write fRegistro0021;
    property Registro0030 : TRegistro0030 read fRegistro0030 write fRegistro0030;
    property Registro0035 : TRegistro0035List read fRegistro0035 write fRegistro0035;
    property Registro0930 : TRegistro0930List read fRegistro0930 write fRegistro0930;
    property Registro0990 : TRegistro0990 read FRegistro0990 write FRegistro0990;

    property Registro0010Count: Integer read FRegistro0010Count write FRegistro0010Count;
    property Registro0020Count: Integer read FRegistro0020Count write FRegistro0020Count;
    property Registro0021Count: Integer read FRegistro0021Count write FRegistro0021Count;
    property Registro0030Count: Integer read FRegistro0030Count write FRegistro0030Count;
    property Registro0035Count: Integer read FRegistro0035Count write FRegistro0035Count;
    property Registro0930Count: Integer read FRegistro0930Count write FRegistro0930Count;                
  end;

implementation

uses
  ACBrTXTUtils, StrUtils, Contnrs;

{ TBloco_0 }

procedure TBloco_0.CriaRegistros;
begin
  inherited ;
  FRegistro0000 := TRegistro0000.Create;
  fRegistro0001 := TRegistro0001.Create;
  fRegistro0010 := TRegistro0010.Create;
  fRegistro0020 := TRegistro0020.Create;
  fRegistro0021 := TRegistro0021.Create;
  fRegistro0030 := TRegistro0030.Create;
  fRegistro0035 := TRegistro0035List.Create;
  fRegistro0930 := TRegistro0930List.Create;
    
  FRegistro0990 := TRegistro0990.Create;

  FRegistro0010Count := 0;
  FRegistro0020Count := 0;
  FRegistro0021Count := 0;
  FRegistro0030Count := 0;
  FRegistro0035Count := 0;
  FRegistro0930Count := 0;

  FRegistro0990.QTD_LIN := 0;
end;

procedure TBloco_0.LiberaRegistros;
begin
  inherited ;
  FRegistro0000.Free;
  fRegistro0001.Free;
  FRegistro0990.Free;
end;

constructor TBloco_0.Create;
begin
  inherited;
  CriaRegistros;
end;

destructor TBloco_0.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

function TBloco_0.Registro0000New: TRegistro0000;
begin
  Result := FRegistro0000;
end;

function TBloco_0.Registro0001New: TRegistro0001;
begin
  Result := FRegistro0001;
end;

function TBloco_0.Registro0010New: TRegistro0010;
begin
  Result := FRegistro0010;
end;

function TBloco_0.Registro0020New: TRegistro0020;
begin
  Result := FRegistro0020;
end;

function TBloco_0.Registro0021New: TRegistro0021;
begin
  Result := FRegistro0021;
end;

function TBloco_0.Registro0030New: TRegistro0030;
begin
  Result := FRegistro0030;
end;

function TBloco_0.Registro0035New: TRegistro0035;
begin
  Result := Registro0001.Registro0035.New();
end;

function TBloco_0.Registro0930New: TRegistro0930;
begin
  Result := Registro0001.Registro0930.New();
end;

procedure TBloco_0.WriteRegistro0000;
var
  strCOD_VER : String;
begin
  if Assigned(FRegistro0000) then
  begin
    with FRegistro0000 do
    begin
	  strCOD_VER := CodVerToStr(COD_VER);

      Add( LFill('0000') +
           LFill('LECF') +
           LFill(strCOD_VER) +
           LFill(CNPJ) +
           LFill(NOME) +
           LFill(IND_SIT_INI_PER) +
           LFill(SIT_ESPECIAL) +
           LFill(PAT_REMAN_CIS) +
           LFill(DT_SIT_ESP) +
           LFill(DT_INI) +
           LFill(DT_FIN) +
           LFill(RETIFICADORA) +
           LFill(NUM_REC) +
           LFill(TIP_ECF) +
           LFill(COD_SCP) );
      FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
    end;
  end;
end;

procedure  TBloco_0.WriteRegistro0001;
begin
  if Assigned(FRegistro0001) then
  begin
    with FRegistro0001 do
    begin
      Add( LFill( '0001' ) +
           LFill( Integer(IND_DAD), 0 ) ) ;

      if IND_DAD = idComDados then
      begin
        WriteRegistro0010;
        WriteRegistro0020;
        WriteRegistro0021;
        WriteRegistro0030;
        WriteRegistro0035(FRegistro0001);
        WriteRegistro0930(FRegistro0001);
      end;
    end;
    Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
  end;
end;

procedure  TBloco_0.WriteRegistro0010;
var
  strOPT_REFIS, strOPT_PAES, strFORMA_TRIB, strIND_REC_RECEITA : string;
begin
  if Assigned(FRegistro0010) then
  begin
    with FRegistro0010 do
    begin
      case OPT_REFIS of
        idSim: strOPT_REFIS := 'S';
        idNao: strOPT_REFIS := 'N';
      end;

      case OPT_PAES of
        idSim: strOPT_PAES := 'S';
        idNao: strOPT_PAES := 'N';
      end;

      case FORMA_TRIB of
        ftlLucroReal                   : strFORMA_TRIB := '1';
        ftlLucroRealArbitrado          : strFORMA_TRIB := '2';
        ftlLucroPresumidoReal          : strFORMA_TRIB := '3';
        ftlLucroPresumidoRealArbitrado : strFORMA_TRIB := '4';
        ftlLucroPresumido              : strFORMA_TRIB := '5';
        ftlLucroArbitrado              : strFORMA_TRIB := '6';
        ftlLucroPresumidoArbitrado     : strFORMA_TRIB := '7';
        ftlImuneIRPJ                   : strFORMA_TRIB := '8';
        ftlIsentoIRPJ                  : strFORMA_TRIB := '9';
      end;

      case IND_REC_RECEITA of
        irrNenhum            : strIND_REC_RECEITA := '';
        irrRegimeCaixa       : strIND_REC_RECEITA := '1';
        irrRegimeCompetencia : strIND_REC_RECEITA := '2';
      end;

      case FRegistro0000.COD_VER of
        ECFVersao100, ECFVersao200:
          Add( LFill('0010') +
               LFill(HASH_ECF_ANTERIOR) +
               LFill(strOPT_REFIS) +
               LFill(strOPT_PAES) +
               LFill(strFORMA_TRIB) +
               LFill(FORMA_APUR) +
               LFill(COD_QUALIF_PJ, 2) +
               LFill(FORMA_TRIB_PER) +
               LFill(MES_BAL_RED) +
               LFill(TIP_ESC_PRE) +
               LFill(TIP_ENT) +
               LFill(FORMA_APUR_I) +
               LFill(APUR_CSLL) +
               LFill(OPT_EXT_RTT) +
               LFill(DIF_FCONT) );

        ECFVersao300, ECFVersao400, ECFVersao500, ECFVersao600:
          Add( LFill('0010') +
               LFill(HASH_ECF_ANTERIOR) +
               LFill(strOPT_REFIS) +
               LFill(strOPT_PAES) +
               LFill(strFORMA_TRIB) +
               LFill(FORMA_APUR) +
               LFill(COD_QUALIF_PJ) + 
               LFill(FORMA_TRIB_PER) +
               LFill(MES_BAL_RED) +
               LFill(TIP_ESC_PRE) +
               LFill(TIP_ENT) +
               LFill(FORMA_APUR_I) +
               LFill(APUR_CSLL) +
               LFill(strIND_REC_RECEITA) );
			   
      // Mantido ECFVersao700 em diante no else para não precisar de nova manutenção
      //   enquanto o leiaute não mudar
      else
          Add( LFill('0010') +
               LFill(HASH_ECF_ANTERIOR) +
               LFill(strOPT_REFIS) +
               LFill(strFORMA_TRIB) +
               LFill(FORMA_APUR) +
               LFill(COD_QUALIF_PJ) + 
               LFill(FORMA_TRIB_PER) +
               LFill(MES_BAL_RED) +
               LFill(TIP_ESC_PRE) +
               LFill(TIP_ENT) +
               LFill(FORMA_APUR_I) +
               LFill(APUR_CSLL) +
               LFill(strIND_REC_RECEITA) );

      end;

      FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
    end;
  end;
end;

procedure  TBloco_0.WriteRegistro0020;
begin
   if Assigned(FRegistro0020) then
   begin
      with FRegistro0020 do
      begin
         case FRegistro0000.COD_VER of
           ECFVersao100, ECFVersao200:
              Add( LFill('0020') +
                   LFill(IND_ALIQ_CSLL) +
                   LFill(IND_QTE_SCP, 3) +
                   LFill(IND_ADM_FUN_CLU) +
                   LFill(IND_PART_CONS) +
                   LFill(IND_OP_EXT) +
                   LFill(IND_OP_VINC) +
                   LFill(IND_PJ_ENQUAD) +
                   LFill(IND_PART_EXT) +
                   LFill(IND_ATIV_RURAL) +
                   LFill(IND_LUC_EXP) +
                   LFill(IND_RED_ISEN) +
                   LFill(IND_FIN) +
                   LFill(IND_DOA_ELEIT) +
                   LFill(IND_PART_COLIG) +
                   LFill(IND_VEND_EXP) +
                   LFill(IND_REC_EXT) +
                   LFill(IND_ATIV_EXT) +
                   LFill(IND_COM_EXP) +
                   LFill(IND_PGTO_EXT) +
                   LFill(IND_ECOM_TI) +
                   LFill(IND_ROY_REC) +
                   LFill(IND_ROY_PAG) +
                   LFill(IND_REND_SERV) +
                   LFill(IND_PGTO_REM) +
                   LFill(IND_INOV_TEC) +
                   LFill(IND_CAP_INF) +
                   LFill(IND_PJ_HAB) +
                   LFill(IND_POLO_AM) +
                   LFill(IND_ZON_EXP) +
                   LFill(IND_AREA_COM)  );

           ECFVersao300:
              Add( LFill('0020') +
                   LFill(IND_ALIQ_CSLL) +
                   LFill(IND_QTE_SCP, 3) +
                   LFill(IND_ADM_FUN_CLU) +
                   LFill(IND_PART_CONS) +
                   LFill(IND_OP_EXT) +
                   LFill(IND_OP_VINC) +
                   LFill(IND_PJ_ENQUAD) +
                   LFill(IND_PART_EXT) +
                   LFill(IND_ATIV_RURAL) +
                   LFill(IND_LUC_EXP) +
                   LFill(IND_RED_ISEN) +
                   LFill(IND_FIN) +
                   LFill(IND_DOA_ELEIT) +
                   LFill(IND_PART_COLIG) +
                   LFill(IND_VEND_EXP) +
                   LFill(IND_REC_EXT) +
                   LFill(IND_ATIV_EXT) +
                   LFill(IND_COM_EXP) +
                   LFill(IND_PGTO_EXT) +
                   LFill(IND_ECOM_TI) +
                   LFill(IND_ROY_REC) +
                   LFill(IND_ROY_PAG) +
                   LFill(IND_REND_SERV) +
                   LFill(IND_PGTO_REM) +
                   LFill(IND_INOV_TEC) +
                   LFill(IND_CAP_INF) +
                   LFill(IND_PJ_HAB) +
                   LFill(IND_POLO_AM) +
                   LFill(IND_ZON_EXP) +
                   LFill(IND_AREA_COM) +
                   LFill(IND_PAIS_A_PAIS));

           ECFVersao400, ECFVersao500,ECFVersao600:
              Add( LFill('0020') +
                   LFill(IND_ALIQ_CSLL) +
                   LFill(IND_QTE_SCP, 3) +
                   LFill(IND_ADM_FUN_CLU) +
                   LFill(IND_PART_CONS) +
                   LFill(IND_OP_EXT) +
                   LFill(IND_OP_VINC) +
                   LFill(IND_PJ_ENQUAD) +
                   LFill(IND_PART_EXT) +
                   LFill(IND_ATIV_RURAL) +
                   LFill(IND_LUC_EXP) +
                   LFill(IND_RED_ISEN) +
                   LFill(IND_FIN) +
                   LFill(IND_DOA_ELEIT) +
                   LFill(IND_PART_COLIG) +
                   LFill(IND_VEND_EXP) +
                   LFill(IND_REC_EXT) +
                   LFill(IND_ATIV_EXT) +
                   LFill(IND_COM_EXP) +
                   LFill(IND_PGTO_EXT) +
                   LFill(IND_ECOM_TI) +
                   LFill(IND_ROY_REC) +
                   LFill(IND_ROY_PAG) +
                   LFill(IND_REND_SERV) +
                   LFill(IND_PGTO_REM) +
                   LFill(IND_INOV_TEC) +
                   LFill(IND_CAP_INF) +
                   LFill(IND_PJ_HAB) +
                   LFill(IND_POLO_AM) +
                   LFill(IND_ZON_EXP) +
                   LFill(IND_AREA_COM) +
                   LFill(IND_PAIS_A_PAIS) +
                   LFill(IND_DEREX));

           ECFVersao700, ECFVersao800,ECFVersao900:
              Add( LFill('0020') +
                   LFill(IND_ALIQ_CSLL) +
                   LFill(IND_QTE_SCP, 3) +
                   LFill(IND_ADM_FUN_CLU) +
                   LFill(IND_PART_CONS) +
                   LFill(IND_OP_EXT) +
                   LFill(IND_OP_VINC) +
                   LFill(IND_PJ_ENQUAD) +
                   LFill(IND_PART_EXT) +
                   LFill(IND_ATIV_RURAL) +
                   LFill(IND_LUC_EXP) +
                   LFill(IND_RED_ISEN) +
                   LFill(IND_FIN) +
                   LFill(IND_PART_COLIG) +
                   LFill(IND_REC_EXT) +
                   LFill(IND_ATIV_EXT) +
                   LFill(IND_PGTO_EXT) +
                   LFill(IND_ECOM_TI) +
                   LFill(IND_ROY_REC) +
                   LFill(IND_ROY_PAG) +
                   LFill(IND_REND_SERV) +
                   LFill(IND_PGTO_REM) +
                   LFill(IND_INOV_TEC) +
                   LFill(IND_CAP_INF) +
                   LFill(IND_PJ_HAB) +
                   LFill(IND_POLO_AM) +
                   LFill(IND_ZON_EXP) +
                   LFill(IND_AREA_COM) +
                   LFill(IND_PAIS_A_PAIS) +
                   LFill(IND_DEREX));
           else // ECFVersao1000
              Add( LFill('0020') +
                   LFill(IND_ALIQ_CSLL) +
                   LFill(IND_QTE_SCP, 3) +
                   LFill(IND_ADM_FUN_CLU) +
                   LFill(IND_PART_CONS) +
                   LFill(IND_OP_EXT) +
                   LFill(IND_OP_VINC) +
                   LFill(IND_PJ_ENQUAD) +
                   LFill(IND_PART_EXT) +
                   LFill(IND_ATIV_RURAL) +
                   LFill(IND_LUC_EXP) +
                   LFill(IND_RED_ISEN) +
                   LFill(IND_FIN) +
                   LFill(IND_PART_COLIG) +
                   LFill(IND_REC_EXT) +
                   LFill(IND_ATIV_EXT) +
                   LFill(IND_PGTO_EXT) +
                   LFill(IND_ECOM_TI) +
                   LFill(IND_ROY_REC) +
                   LFill(IND_ROY_PAG) +
                   LFill(IND_REND_SERV) +
                   LFill(IND_PGTO_REM) +
                   LFill(IND_INOV_TEC) +
                   LFill(IND_CAP_INF) +
                   LFill(IND_PJ_HAB) +
                   LFill(IND_POLO_AM) +
                   LFill(IND_ZON_EXP) +
                   LFill(IND_AREA_COM) +
                   LFill(IND_PAIS_A_PAIS) +
                   LFill(IND_DEREX) +
                   LFill(IND_PR_TRANSF));
         end;
         FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
      end;
   end;
end;

procedure TBloco_0.WriteRegistro0021;
begin
  if Assigned(FRegistro0021)
    and (fRegistro0020.IND_PJ_HAB='S') then // Validação cfe manual SPED ECF
  begin
    with FRegistro0021 do
    begin
      Add( LFill('0021') +
           LFill(IND_REPES) +
           LFill(IND_RECAP) +
           LFill(IND_PADIS) +
           LFill(IND_PATVD) +
           LFill(IND_REIDI) +
           LFill(IND_REPENEC) +
           LFill(IND_REICOMP) +
           LFill(IND_RETAERO) +
           LFill(IND_RECINE) +
           LFill(IND_RESIDUOS_SOLIDOS) +
           LFill(IND_RECOPA) +
           LFill(IND_COPA_DO_MUNDO) +
           LFill(IND_RETID) +
           LFill(IND_REPNBL_REDES) +
           LFill(IND_REIF) +
           LFill(IND_OLIMPIADAS) );
    end;
    FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
  end;
end;

procedure  TBloco_0.WriteRegistro0030;
begin
  if Assigned(FRegistro0030) then
  begin
    with FRegistro0030 do
    begin
      Add( LFill('0030') +
           LFill(COD_NAT) +
           LFill(CNAE_FISCAL) +
           LFill(ENDERECO) +
           LFill(NUM) +
           LFill(COMPL) +
           LFill(BAIRRO) +
           LFill(UF) +
           LFill(COD_MUN) +
           LFill(CEP) +
           LFill(NUM_TEL) +
           LFill(EMAIL) );
      FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
    end;
  end;
end;

procedure  TBloco_0.WriteRegistro0035(Reg0001: TRegistro0001);
var
  intfor : Integer;
begin
  if Assigned(Reg0001.Registro0035) then
  begin
    for intfor := 0 to Reg0001.Registro0035.Count - 1 do
    begin
      with Reg0001.Registro0035.Items[intfor] do
      begin
        Add( LFill('0035') +
             LFill(COD_SCP) +
             LFill(NOME_SCP) );
        FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
      end;
    end;
    FRegistro0035Count := FRegistro0035Count + Reg0001.Registro0035.Count;
  end;
end;

procedure  TBloco_0.WriteRegistro0930(Reg0001: TRegistro0001) ;
var
  strIDENT_QUALIF : string;
  intfor : integer;
begin
  if Assigned(Reg0001.Registro0930) then
  begin
    for intfor := 0 to Reg0001.Registro0930.Count - 1 do
    begin
      with Reg0001.Registro0930.items[intfor] do
      begin
        case IDENT_QUALIF of
          qaDiretor: strIDENT_QUALIF := '203';
          qaConselheirodeAdministracao: strIDENT_QUALIF := '204';
          qaAdministrador: strIDENT_QUALIF := '205';
          qaAdministradordoGrupo: strIDENT_QUALIF := '206';
          qaAdministradordeSociedadeFiliada: strIDENT_QUALIF := '207';
          qaAdministradorJudicialPF: strIDENT_QUALIF := '222';
          qaAdministradorJudicialPJ: strIDENT_QUALIF := '223';
          qaAdministradorJudicialGestor: strIDENT_QUALIF := '226';
          qaProcurador: strIDENT_QUALIF := '309';
          qaInventariante: strIDENT_QUALIF := '312';
          qaLiquidante: strIDENT_QUALIF := '313';
          qaInterventor: strIDENT_QUALIF := '315';
          qaTitualarPF: strIDENT_QUALIF := '401';
          qaEmpresario: strIDENT_QUALIF := '801';
          qaContador: strIDENT_QUALIF := '900';
          qaContabilista: strIDENT_QUALIF := '900';
          qaOutros: strIDENT_QUALIF := '999';
        end;

        Add( LFill('0930') +
             LFill(IDENT_NOM) +
             LFill(IDENT_CPF_CNPJ) +
             LFill(strIDENT_QUALIF) +
             LFill(IND_CRC) +
             LFill(EMAIL) +
             LFill(FONE) );
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
    end;
    FRegistro0930Count := FRegistro0930Count + Reg0001.Registro0930.Count;
  end;
end;

procedure  TBloco_0.WriteRegistro0990;
begin
  if Assigned(Registro0990) then
  begin
    with Registro0990 do
    begin
      QTD_LIN_0 := QTD_LIN_0 + 1;
       Add( LFill('0990') +
            LFill(QTD_LIN_0,0) );
    end;
  end;
end;

procedure TBloco_0.LimpaRegistros;
begin
  LiberaRegistros;
  Conteudo.Clear;

  CriaRegistros;
end;

end.
