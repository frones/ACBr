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
|* 09/05/2014: Juliano Rosa
|*  - Ajuste COD_PAIS para 5 posições (Registro0150)
|*  - Ajuste StrToIntDef(COD_BF_ISS) (Registro0025)
|*  - Ajuste StrToIntDef(wIND_RT,0)  (Registro0030)
*******************************************************************************}
{$I ACBr.inc}

unit ACBrSEF2_Bloco0_1;

interface

uses
  SysUtils, Classes,
  ACBrSEF2_Bloco0, ACBrSEF2Conversao;

  type TBloco_0 = class(TACBrSEFIIEDOC)
  private
    FRegistro0000: TRegistroSEF0000;
    FRegistro0001: TRegistroSEF0001;
    FRegistro0990: TRegistroSEF0990;

    FRegistro0005Count: Integer;
    FRegistro0025Count: Integer;
    FRegistro0030Count: Integer;
    FRegistro0100Count: Integer;
    FRegistro0150Count: Integer;
    FRegistro0200Count: Integer;
    FRegistro0205Count: Integer;
    FRegistro0215Count: Integer;
    FRegistro0400Count: Integer;
    FRegistro0450Count: Integer;
    FRegistro0460Count: Integer;
    FRegistro0465Count: Integer;
    FRegistro0470Count: Integer;

//    FRegistro0005: TRegistroSEF0005;
    FRegistro0025: TRegistroSEF0025;
//    FRegistro0030: TRegistroSEF0030;
//    FRegistro0100: TRegistroSEF0100;

    FRegistro0150: TRegistroSEF0150List;
//    FRegistro0200: TRegistroSEF0200List;
//    FRegistro0205: TRegistroSEF0205List;
    FRegistro0400: TRegistroSEF0400List;
    FRegistro0450: TRegistroSEF0450List;
    FRegistro0460: TRegistroSEF0460List;
    FRegistro0465: TRegistroSEF0465List;
    FRegistro0470 : TRegistroSEF0470List;


    procedure WriteRegistro0005(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0025(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0030(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0100(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0150(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0200(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0205(Reg0200: TRegistroSEF0200);
    procedure WriteRegistro0215(Reg0200: TRegistroSEF0200);
    procedure WriteRegistro0400(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0450(Reg0001: TRegistroSEF0001);
    procedure WriteRegistro0460(Reg0450: TRegistroSEF0450);
    procedure WriteRegistro0465(Reg0450: TRegistroSEF0450);
    procedure WriteRegistro0470(Reg0450: TRegistroSEF0450);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy;override;
    procedure LimpaRegistros;

    function Registro0000New : TRegistroSEF0000;
    function Registro0001New : TRegistroSEF0001;
    function Registro0005New : TRegistroSEF0005;
    function Registro0025New : TRegistroSEF0025;
    function Registro0030New : TRegistroSEF0030;
    function Registro0100New : TRegistroSEF0100;
    function Registro0150New : TRegistroSEF0150;
    function Registro0200New : TRegistroSEF0200;
    function Registro0205New : TRegistroSEF0205;
    function Registro0400New : TRegistroSEF0400;
    function Registro0450New : TRegistroSEF0450;
    function Registro0460New : TRegistroSEF0460;
    function Registro0465New : TRegistroSEF0465;
    function Registro0470New : TRegistroSEF0470;


    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;

    property Registro0000: TRegistroSEF0000 read FRegistro0000 write FRegistro0000;
    property Registro0001: TRegistroSEF0001 read FRegistro0001 write FRegistro0001;
    property Registro0990: TRegistroSEF0990 read FRegistro0990 write FRegistro0990;

    property Registro0005Count: Integer read FRegistro0005Count write FRegistro0005Count;
    property Registro0025Count: Integer read FRegistro0025Count write FRegistro0025Count;
    property Registro0030Count: Integer read FRegistro0030Count write FRegistro0030Count;
    property Registro0100Count: Integer read FRegistro0100Count write FRegistro0100Count;
    property Registro0150Count: Integer read FRegistro0150Count write FRegistro0150Count;
    property Registro0200Count: Integer read FRegistro0200Count write FRegistro0200Count;
    property Registro0205Count: Integer read FRegistro0205Count write FRegistro0205Count;
    property Registro0215Count: Integer read FRegistro0215Count write FRegistro0215Count;
    property Registro0400Count: Integer read FRegistro0400Count write FRegistro0400Count;
    property Registro0450Count: Integer read FRegistro0450Count write FRegistro0450Count;
    property Registro0460Count: Integer read FRegistro0460Count write FRegistro0460Count;
    property Registro0465Count: Integer read FRegistro0465Count write FRegistro0465Count;
    property Registro0470Count: Integer read FRegistro0470Count write FRegistro0470Count;

    procedure WriteReg990(var AString : TStringList);
  end;


implementation

{ TBloco0 }

function IntToStrNull(AInteger : Integer) : string;
begin
   if AInteger = 0 then
     Result := ''
   else
     Result := IntToStr(AInteger);
end;

function ConvertCodFinalidade(SEFIICodFinalidade : TSEFIICodFinalidade) : string;
begin
  case SEFIICodFinalidade of
    raOriginal : Result := '0';
    raSubstituto : Result := '1';
  end;
end;

function ConvertVersaoLeiaute(SEFIIVersaoLeiaute : TSEFIIVersaoLeiaute) : string;
begin
  Case SEFIIVersaoLeiaute of
    vlVersao2000 : Result := '2000';
  end;
end;

function ConvertIndicadorConteudo(SEFIIIndicadorConteudo : TSEFIIIndicadorConteudo) : string;
begin
  case SEFIIIndicadorConteudo of
     icContConteudo : Result := '0';
     icSemConteudo : Result := '1';
  end;
end;

function ConvertSEFIIBeniFiscalICMS(SEFIIBeniFiscalICMS : TSEFIIBeniFiscalICMS) : string;
begin
  case SEFIIBeniFiscalICMS of
     bfNenhum : Result := '';
     bfProdepe : Result := 'PE001';
  end;
end;

function ConvertSEFIIConteudoArquivo(SEFIIConteudoArquivo : TSEFIIConteudArquivo) : string;
begin
  case SEFIIConteudoArquivo of
     caLancOpResultFiscal     : Result := '20';
     caLancControlesFiscais   : Result := '21';
     caExtratodocfiscais      : Result := '91';

  end;
end;

function ConvertSEFIIIndicadorDocArregadacao(SEFIIIndicadorDocArregadacao :  TSEFIIIndicadorDocArregadacao) : string;
begin
  case SEFIIIndicadorDocArregadacao of
    daEstadualDistrital : Result := '0';
    daGuiaNasRecEstadual : Result := '1';
    daDocArrecadacaoMunicipal : Result := '2';
    daDocArrecadacaoFederal : Result := '3';
    daOutros : Result := '9';
  end;
end;

function ConvertSEFIIQualiAssinante(SEFIIQualiAssinante : TSEFIIQualiAssinante) : String;
begin
  case SEFIIQualiAssinante of
    qaDiretor                   : Result := '203'; // 203	Diretor
    qaConsAdministracao         : Result := '204'; // 204	Conselheiro de administração
    qaAdministrador             : Result := '205'; // 205	Administrador
    qaAdmGrupo                  : Result := '206'; // 206	Administrador de grupo
    qaAdmSociedadeFiliada       : Result := '207'; // 207	Administrador de sociedade filiada
    qaAdmJudicialPessoaFisica   : Result := '220'; // 220	Administrador judicial - pessoa física
    qaAdmJudicialPessoaJuridica : Result := '220'; // 222	Administrador judicial - pessoa jurídica/profissional responsável
    qaAdmJudicial               : Result := '223'; // 223	Administrador judicial - gestor
    qaGestorJudicial            : Result := '226'; // 226	Gestor judicial
    qaProcurador                : Result := '309'; // 309	Procurador
    qaInventariante             : Result := '312'; // 312	Inventariante
    qaLiquidante                : Result := '313'; // 313	Liquidante
    qaInterventor               : Result := '315'; // 315	Interventor
    qaEmpresario                : Result := '801'; // 801	Empresário
    qaContador                  : Result := '900'; // 900	Contador
    qaOutros                    : Result := '999'; // 999	Outros
  end;
end;

function ConvertSEFIIDocFiscalReferenciado(SEFIIDocFiscalReferenciado : TSEFIIDocFiscalReferenciado) : string;
begin
  Case SEFIIDocFiscalReferenciado of
    SrefNF : Result := '01';
    SrefNFVCCVC : Result := '02';
    SrefCCF : Result := '2D';
    SrefCBP : Result := '2E';
    SrefNFPR : Result := '04';
    SrefNFEE : Result := '06';
    SrefNFTR : Result := '07';
    SrefCTRC : Result := '08';
    SrefCTAQ : Result := '09';
    SrefCTAR : Result := '10';
    SrefCTFC : Result := '11';
    SrefBPR : Result := '13';
    SrefBPAQ : Result := '14';
    SrefBPNB : Result := '15';
    SrefBPF : Result := '16';
    SrefDT : Result := '17';
    SrefRMD : Result := '18';
    SrefOCC : Result := '20';
    SrefNFSC : Result := '21';
    SrefNFST : Result := '22';
    SrefGNRE : Result := '23';
    SrefACT : Result := '24';
    SrefMC : Result := '25';
    SrefCTMC : Result := '26';
    SrefNFTF : Result := '27';
    SrefNFGC : Result := '28';
    SrefNFAC : Result := '29';
    SrefMV : Result := '30';
    SrefBRP : Result := '31';
    SrefNFe : Result := '55';
    SrefCTe : Result := '57';
  end;
end;

constructor TBloco_0.Create;
begin
  inherited;
  CriaRegistros
end;

destructor TBloco_0.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_0.WriteRegistro0000;
var cCOD_CTD : string;
begin
  if Assigned(Registro0000) then
  begin
     with Registro0000 do
     begin
       cCOD_CTD :=  ConvertSEFIIConteudoArquivo(COD_CTD);
        Add( LFill( '0000' )     +
             LFill( 'LFPD' )     +
             LFill( DT_INI )     +
             LFill( DT_FIN )     +
             LFill( NOME_EMPR )  +
             LFill( CNPJ )       +
             LFill( UF )         +
             LFill( IE )         +
             LFill( COD_MUN, 7 ) +
             LFill( IM )         +
             LFill('')           +
             LFill( SUFRAMA )    +
             LFill('2000')       +
             LFill('0')          +
             LFill( cCOD_CTD )   +
             LFill( PAIS )       +
             LFill( FANTASIA )   +
             LFill( NIRE,11, True )    +
             LFill( CPF )        +
             LFill(''));

       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

function TBloco_0.Registro0000New: TRegistroSEF0000;
begin
  Result := FRegistro0000;
end;

function TBloco_0.Registro0001New: TRegistroSEF0001;
begin
  Result := FRegistro0001;
end;

procedure TBloco_0.WriteRegistro0001;
begin
   if Assigned(FRegistro0001) then
   begin
      with FRegistro0001 do
      begin
         Add( LFill( '0001' ) +
              LFill( Integer(IND_MOV), 0 ) ) ;

         if IND_MOV = icContConteudo then
         begin
            WriteRegistro0005(FRegistro0001) ;
            WriteRegistro0025(FRegistro0001) ;
            WriteRegistro0030(FRegistro0001) ;
            WriteRegistro0100(FRegistro0001) ;
            WriteRegistro0150(FRegistro0001) ;
            WriteRegistro0200(FRegistro0001) ; //Somente eDoc
            WriteRegistro0400(FRegistro0001) ;
            WriteRegistro0450(FRegistro0001) ;
         end;
      end;

      Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
   end;
end;

function TBloco_0.Registro0100New: TRegistroSEF0100;
begin
  Result := FRegistro0001.Registro0100;
end;

procedure TBloco_0.WriteRegistro0100(Reg0001: TRegistroSEF0001);
var
  wCodAssin: String;
begin
   if Assigned(Reg0001.Registro0100) then
   begin
     with Reg0001.Registro0100 do
     begin
        wCodAssin :=  ConvertSEFIIQualiAssinante(COD_ASSIN);
        Add( LFill('0100')        +
             LFill(NOME)          +
             LFill(wCodAssin)     +
             LFill(CNPJ)          +
             LFill(CPF)           +
             LFill(CRC)           +
             LFill(CEP,8)         +
             LFill(ENDERECO)      +
             LFill(NUM)           +
             LFill(COMPL)         +
             LFill(BAIRRO)        +
             LFill(UF)            +
             LFill(COD_MUN,7)     +
             LFill(CEP_CP,8,True) +
             LFill(CP)       +
             LFill(FONE)     +
             LFill(FAX)      +
             LFill(EMAIL)) ;

        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0150(Reg0001: TRegistroSEF0001);
var
 intFor : Integer;
begin
   if Assigned(Reg0001.Registro0150) then
   begin
      for intFor := 0 to Reg0001.Registro0150.Count - 1 do
      begin
         with Reg0001.Registro0150.Items[intFor] do
         begin
            Add( LFill('0150')    +
                 LFill(COD_PART)  +
                 LFill(NOME)      +
                 LFill(COD_PAIS,5)  +
                 LFill(CNPJ)      +
                 LFill(CPF)       +
                 LFill('')        +
                 LFill(UF)        +
                 LFill(IE)        +
                 LFill(IE_ST)     +
                 LFill(COD_MUN,7) +
                 LFill(IM)        +
                 LFill(SUFRAMA) );
         end;
         /// Registros FILHOS
         //WriteRegistro0175( Reg0001.Registro0150.Items[intFor] );

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0150Count := FRegistro0150Count + Reg0001.Registro0150.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0400(Reg0001: TRegistroSEF0001);
var
  intFor : Integer;
begin
   if Assigned(Reg0001.Registro0400) then
   begin
      for intFor := 0 to Reg0001.Registro0400.Count - 1 do
      begin
         with Reg0001.Registro0400.Items[intFor] do
         begin
            Add( LFill('0400')      +
                 LFill( COD_NAT )   +
                 LFill( DESCR_NAT ) +
                 LFill( COP )) ;
         end;
         
         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0400Count := FRegistro0400Count + Reg0001.Registro0400.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0450(Reg0001: TRegistroSEF0001);
var
 intFor : Integer;
begin
   if Assigned( Reg0001.Registro0450 ) then
   begin
      for intFor := 0 to Reg0001.Registro0450.Count - 1 do
      begin
         with Reg0001.Registro0450.Items[intFor] do
         begin
            Add( LFill('0450')    +
                 LFill( COD_INF ) +
                 LFill( TXT ) ) ;
         end;

         WriteRegistro0460( Reg0001.Registro0450.Items[intFor] ) ;
         WriteRegistro0465( Reg0001.Registro0450.Items[intFor] ) ;
         WriteRegistro0470( Reg0001.Registro0450.Items[intFor] ) ;
         
         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0450Count := FRegistro0450Count + Reg0001.Registro0450.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0460(Reg0450: TRegistroSEF0450);
var
 intFor : Integer;
begin
   if Assigned( Reg0450.Registro0460 ) then
   begin
      for intFor := 0 to Reg0450.Registro0460.Count - 1 do
      begin
         with Reg0450.Registro0460.Items[intFor] do
         begin
            Add( LFill('0460')   +
                 LFill(ConvertSEFIIIndicadorDocArregadacao(AIND_DA))   +
                 LFill(DESCR_DA) +
                 LFill(UF)       +
                 LFill(COD_MUN)  +
                 LFill(PER_REF)  +
                 LFill(NUM_DA)   +
                 LFill(VL_DA)    +
                 LFill(DT_VCTO)  +
                 LFill(VL_DESC)  +
                 LFill(VL_MOR)   +
                 LFill(VL_JRS)   +
                 LFill(VL_MUL)   +
                 LFill(VL_PGTO)  +
                 LFill(DT_PGTO)  +
                 LFill(AUT_BCO) ) ;
         end;
         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro0460Count := FRegistro0460Count + Reg0450.Registro0460.Count;
  end;
end;


procedure TBloco_0.WriteRegistro0465(Reg0450: TRegistroSEF0450);
var
 intFor : Integer;
begin
   if Assigned( Reg0450.Registro0465 ) then
   begin
      for intFor := 0 to Reg0450.Registro0465.Count - 1 do
      begin
         with Reg0450.Registro0465.Items[intFor] do
         begin

            Add( LFill('0465')                +
                 LFill(IndOperToStr(IND_OPER), 1)  +
                 LFill(IndEmissaoToStr(IND_EMIT), 1)  +
                 LFill(CNPJ)                  +
                 LFill(CPF)                   +
                 LFill(UF)                    +
                 LFill(IE)                    +
                 LFill(COD_MUN, 7)            +
                 LFill(IM)                    +
                 LFill(ModDocumentoToStr(COD_MOD)) +
                 LFill(CodSituacaoToStr(COD_SIT),2)  +
                 LFill(SER)                   +
                 LFill(SUB)                   +
                 LFill(CHV_NFE_CTE)           +
                 LFill(NUM_DOC)               +
                 LFill(DT_DOC)                +
                 LFill(VL_DOC,2)              +
                 LFill(VL_ISS,0, 2, true)     +
                 LFill(VL_RT,0, 2, true)      +
                 LFill(VL_ICMS,2)             +
                 LFill(VL_ICMS_ST,0, 2, true) +
                 LFill(VL_AT,0, 2, true)      +
                 LFill(VL_IPI,2)              +
                 LFill(VOL,2));
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0465Count := FRegistro0465Count + Reg0450.Registro0465.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0470(Reg0450: TRegistroSEF0450);
var
 intFor : Integer;
begin
   if Assigned( Reg0450.Registro0470 ) then
   begin
      for intFor := 0 to Reg0450.Registro0470.Count - 1 do
      begin
         with Reg0450.Registro0470.Items[intFor] do
         begin
            Add( LFill('0470')   +
                 LFill(ModDocumentoToStr(COD_MOD))  +
                 LFill(ECF_CX, 0)   +
                 LFill(ECF_FAB)     +
                 LFill(CRO, 0)      +
                 LFill(CRZ, 0)      +
                 LFill(NUM_DOC, 0)  +
                 LFill(DT_DOC)   +
                 LFill(VL_DOC, 2)   +
                 LFill(VL_ISS, 2)   +
                 LFill(VL_ICMS, 2));
         end;
         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro0470Count := FRegistro0470Count + Reg0450.Registro0470.Count;
   end;
end;

procedure TBloco_0.WriteReg990(var AString: TStringList);
var iTotal : Integer;
begin
  iTotal := 5;
  iTotal := iTotal + FRegistro0150.Count;
  iTotal := iTotal + FRegistro0400.Count;
  iTotal := iTotal + FRegistro0450.Count;
  iTotal := iTotal + FRegistro0460.Count;
  iTotal := iTotal + FRegistro0465.Count;
  iTotal := iTotal + FRegistro0470.Count;
  AString.Add('|0990|'+IntToStr(iTotal)+'|');
end;

procedure TBloco_0.WriteRegistro0005(Reg0001: TRegistroSEF0001);
var
 wCOD_ASSIN :String;
begin
   if Assigned(Reg0001.Registro005) then
   begin
      with Reg0001.Registro005 do
      begin
         wCOD_ASSIN:= ConvertSEFIIQualiAssinante(COD_ASSIN);
         Add( LFill('0005')         +
              LFill(NOME_RESP)      +
              LFill(wCOD_ASSIN)     +
              LFill(CPF_RESP)       +
              LFill(CEP, 8)         +
              LFill(ENDERECO)       +
              LFill(NUM)            +
              LFill(COMPL)          +
              LFill(BAIRRO)         +
              LFill(CEP_CP, 8,True) +
              LFill(CP)             +
              LFill(FONE)           +
              LFill(FAX)            +
              LFill(EMAIL) ) ;

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;

      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro0005Count := FRegistro0005Count + 1;
   end;
end;

procedure TBloco_0.WriteRegistro0200(Reg0001: TRegistroSEF0001);
var
 intFor : Integer;
 strLinha :String;
begin
   if Assigned( Reg0001.Registro0200 ) then
   begin
      //-- Before
      strLinha := '';
      {if Assigned(FOnBeforeWriteRegistro0200) then
      begin
         FOnBeforeWriteRegistro0200(strLinha);
         if strLinha <> EmptyStr then
            Add(strLinha);
      end;}

      for intFor := 0 to Reg0001.Registro0200.Count - 1 do
      begin
         strLinha := '';
         with Reg0001.Registro0200.Items[intFor] do
         begin
            strLinha := LFill('0200')       +
                        LFill( COD_ITEM )   +
                        LFill( DESCR_ITEM ) +
                        LFill( COD_GEN, 2 ) +
                        LFill( COD_LST, 4, True, '0' );
            //-- Write
            {if Assigned(FOnWriteRegistro0200) then
               FOnWriteRegistro0200(strLinha); }

            Add(strLinha);
         end;

         /// Registros FILHOS
         WriteRegistro0205( Reg0001.Registro0200.Items[intFor] ) ;
         //WriteRegistro0210( Reg0001.Registro0200.Items[intFor] ) ;
         WriteRegistro0215( Reg0001.Registro0200.Items[intFor] );

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
      //-- After
      strLinha := '';
      {if Assigned(FOnAfterWriteRegistro0200) then
      begin
         FOnAfterWriteRegistro0200(strLinha);
         if strLinha <> EmptyStr then
            Add(strLinha);
      end;}

     /// Variável para armazenar a quantidade de registro do tipo.
     FRegistro0200Count := FRegistro0200Count + Reg0001.Registro0200.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0205(Reg0200: TRegistroSEF0200);
var
  intFor : Integer;
begin
   if Assigned( Reg0200.Registro0205 ) then
   begin
      for intFor := 0 to Reg0200.Registro0205.Count - 1 do
      begin
         with Reg0200.Registro0205.Items[intFor] do
         begin
          Add( LFill('0205')           +
               LFill( COD_ITEM_ANT )   +
               LFill( DESCR_ITEM_ANT ) +
               LFill( DT_FIN )         +
               LFill( DT_FIN) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0205Count := FRegistro0205Count + Reg0200.Registro0205.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0215(Reg0200: TRegistroSEF0200);
var
  intFor: Integer;
begin
  if Assigned( Reg0200.Registro0215 ) then
  begin
    for intFor := 0 to Reg0200.Registro0215.Count - 1 do
    begin
       with Reg0200.Registro0215.Items[intFor] do
       begin
         Add(LFill('0215')           +
             LFill( COD_ITEM_ANP )   +
             LFill( DT_INI )         +
             LFill( DT_FIN) ) ;
       end;
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro0215Count := FRegistro0215Count + Reg0200.Registro0215.Count;
  end;
end;

procedure TBloco_0.CriaRegistros;
begin
  FRegistro0000 := TRegistroSEF0000.Create;
  FRegistro0001 := TRegistroSEF0001.Create;
  FRegistro0990 := TRegistroSEF0990.Create;

  FRegistro0005Count := 0;
  FRegistro0025Count := 0;
  FRegistro0030Count := 0;
  FRegistro0100Count := 0;
  FRegistro0150Count := 0;
  FRegistro0200Count := 0;
  FRegistro0205Count := 0;
  FRegistro0215Count := 0;
  FRegistro0400Count := 0;
  FRegistro0450Count := 0;
  FRegistro0460Count := 0;
  FRegistro0465Count := 0;
  FRegistro0470Count := 0;

  FRegistro0990.QTD_LIN_0 := 0;
end;

procedure TBloco_0.LiberaRegistros;
begin
   FRegistro0000.Free;
   FRegistro0001.Free;
   FRegistro0990.Free;
end;

procedure TBloco_0.LimpaRegistros;
begin
   /// Limpa os Registros
   LiberaRegistros;
   Conteudo.Clear;

   /// Recriar os Registros Limpos
   CriaRegistros;
end;

procedure TBloco_0.WriteRegistro0025(Reg0001: TRegistroSEF0001);
begin
   if Assigned(Reg0001.Registro025) then
   begin
      with Reg0001.Registro025 do
      begin
         Add( LFill('0025')     +
              LFill(ConvertSEFIIBeniFiscalICMS(COD_BF_ICMS))+
              LFill(StrToIntDef(COD_BF_ISS,0),0) //LFill(Integer(COD_BF_ISS),0)
             );

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;

      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro0025Count := FRegistro0025Count + 1;
   end;
end;

function TBloco_0.Registro0005New: TRegistroSEF0005;
begin
   Result := FRegistro0001.Registro005;
end;

procedure TBloco_0.WriteRegistro0990;
var
 strLinha : AnsiString;
begin
   //--Before
   strLinha := '';
   {if Assigned(FOnBeforeWriteRegistro0990) then
   begin
      FOnBeforeWriteRegistro0990(strLinha);
      if strLinha <> EmptyStr then
         Add(strLinha);
   end;}

   if Assigned(Registro0990) then
   begin
      with Registro0990 do
      begin
         QTD_LIN_0 := QTD_LIN_0 + 1;
         strLinha := LFill('0990') +
                     LFill(QTD_LIN_0,0);

         //if Assigned(FOnWriteRegistro0990) then FOnWriteRegistro0990(strLinha);

         Add(strLinha);
      end;
   end;

   //-- After
   strLinha := '';
   {if Assigned(FOnAfterWriteRegistro0990) then
   begin
      FOnAfterWriteRegistro0990(strLinha);
      if strLinha <> EmptyStr then
         Add(strLinha);
   end;}
end;

function TBloco_0.Registro0025New: TRegistroSEF0025;
begin
   FRegistro0025 := TRegistroSEF0025.Create(FRegistro0001);
   Result := FRegistro0001.Registro025;
end;

procedure TBloco_0.WriteRegistro0030(Reg0001: TRegistroSEF0001);
begin
   if Assigned(Reg0001.Registro030) then
   begin
      with Reg0001.Registro030 do
      begin
         Add( LFill('0030')              +
              LFill(Integer(IND_ED),0)   +
              LFill(Integer(IND_ARQ),0)  +
              LFill(IndExigEscrImpostoToStr(PRF_ISS ),0) +
              LFill(IndExigEscrImpostoToStr(PRF_ICMS),0) +
              LFill(IndExigDiversaToStr(PRF_RIDF),0) +
              LFill(IndExigDiversaToStr(PRF_RUDF),0) +
              LFill(IndExigDiversaToStr(PRF_LMC ),0) +
              LFill(IndExigDiversaToStr(PRF_RV  ),0) +
              LFill(IndExigDiversaToStr(PRF_RI  ),0) +
              LFill(IndEscrContabilToStr(IND_EC ),0) +
              LFill(IndExigDiversaToStr(IND_ISS ),0) +
              LFill(IndExigDiversaToStr(IND_RT  ),0, true) +
              LFill(IndExigDiversaToStr(IND_ICMS),0) +
              LFill(IndExigDiversaToStr(IND_ST  ),0) +
              LFill(IndExigDiversaToStr(IND_AT  ),0) +
              LFill(IndExigDiversaToStr(IND_IPI ),0) +
              LFill(IndExigDiversaToStr(IND_RI  )) ) ;

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;

      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro0030Count := FRegistro0030Count + 1;
   end;
end;

function TBloco_0.Registro0030New: TRegistroSEF0030;
begin
   Result := FRegistro0001.Registro030;
end;

function TBloco_0.Registro0150New: TRegistroSEF0150;
begin
   Result := FRegistro0001.Registro0150.New(FRegistro0001);
end;

function TBloco_0.Registro0200New: TRegistroSEF0200;
begin
   Result := FRegistro0001.Registro0200.New();
end;

function TBloco_0.Registro0205New: TRegistroSEF0205;
var
  U0200: TRegistroSEF0200;
  U0200Count: Integer;
begin
   U0200Count := FRegistro0001.Registro0200.Count -1;
   if U0200Count = -1 then
      raise Exception.Create('O registro 0205 deve ser filho do registro 0200, e não existe nenhum 0200 pai!');

   U0200 := FRegistro0001.Registro0200.Items[U0200Count];
   Result  := U0200.Registro0205.New(U0200);
end;

function TBloco_0.Registro0400New: TRegistroSEF0400;
begin
   Result := FRegistro0001.Registro0400.New();
end;

function TBloco_0.Registro0450New: TRegistroSEF0450;
begin
   Result := FRegistro0001.Registro0450.New();
end;

function TBloco_0.Registro0460New: TRegistroSEF0460;
var
  U0450: TRegistroSEF0450;
  U0450Count: Integer;
begin
   U0450Count := FRegistro0001.Registro0450.Count -1;
   if U0450Count = -1 then
      raise Exception.Create('O registro 0460 deve ser filho do registro 0450, e não existe nenhum 0450 pai!');

   U0450 := FRegistro0001.Registro0450.Items[U0450Count];
   Result  := U0450.Registro0460.New(U0450);
end;

function TBloco_0.Registro0465New: TRegistroSEF0465;
var
  U0450: TRegistroSEF0450;
  U0450Count: Integer;
begin
   U0450Count := FRegistro0001.Registro0450.Count -1;
   if U0450Count = -1 then
      raise Exception.Create('O registro 0465 deve ser filho do registro 0450, e não existe nenhum 0450 pai!');

   U0450 := FRegistro0001.Registro0450.Items[U0450Count];
   Result  := U0450.Registro0465.New(U0450);
end;

function TBloco_0.Registro0470New: TRegistroSEF0470;
var
  U0450: TRegistroSEF0450;
  U0450Count: Integer;
begin
   U0450Count := FRegistro0001.Registro0450.Count -1;
   if U0450Count = -1 then
      raise Exception.Create('O registro 0470 deve ser filho do registro 0450, e não existe nenhum 0450 pai!');

   U0450 := FRegistro0001.Registro0450.Items[U0450Count];
   Result  := U0450.Registro0470.New(U0450);
end;

end.
