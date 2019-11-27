{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simoes de Almeida               }
{                                       Isaque Pinheiro                        }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 15/02/2011: Isaque Pinheiro e Fernando Amado
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEPCBloco_0_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEPCBloco_0, ACBrEPCBlocos;

type
  /// TBLOCO_0 - Abertura, Identificação e Referências

  { TBloco_0 }

  TBloco_0 = class(TACBrSPED)
  private
    FRegistro0000: TRegistro0000;      /// BLOCO 0 - Registro0000
    FRegistro0001: TRegistro0001;      /// BLOCO 0 - Registro0001
    FRegistro0990: TRegistro0990;      /// BLOCO 0 - Registro0990

    FRegistro0035Count: Integer;
    FRegistro0100Count: Integer;
    FRegistro0110Count: Integer;
    FRegistro0111Count: Integer;
    FRegistro0120Count: Integer;   //Adicionado por Fábio Gabriel - 29/11/2012
    FRegistro0140Count: Integer;
    FRegistro0145Count: Integer;
    FRegistro0150Count: Integer;
    FRegistro0190Count: Integer;
    FRegistro0200Count: Integer;
    FRegistro0205Count: Integer;
    FRegistro0206Count: Integer;
    FRegistro0208Count: Integer;
    FRegistro0400Count: Integer;
    FRegistro0450Count: Integer;
    FRegistro0500Count: Integer;
    FRegistro0600Count: Integer;

    procedure WriteRegistro0035(Reg0001: TRegistro0001);
    procedure WriteRegistro0100(Reg0001: TRegistro0001);
    procedure WriteRegistro0110(Reg0001: TRegistro0001);
    procedure WriteRegistro0111(Reg0110: TRegistro0110);
    procedure WriteRegistro0120(Reg0001: TRegistro0001);  //Adicionado por Fábio Gabriel - 29/11/2012
    procedure WriteRegistro0140(Reg0001: TRegistro0001);
    procedure WriteRegistro0145(Reg0140: TRegistro0140);
    procedure WriteRegistro0150(Reg0140: TRegistro0140);
    procedure WriteRegistro0190(Reg0140: TRegistro0140);
    procedure WriteRegistro0200(Reg0140: TRegistro0140);
    procedure WriteRegistro0205(Reg0200: TRegistro0200);
    procedure WriteRegistro0206(Reg0200: TRegistro0200);
    procedure WriteRegistro0208(Reg0200: TRegistro0200);
    procedure WriteRegistro0400(Reg0140: TRegistro0140);
    procedure WriteRegistro0450(Reg0140: TRegistro0140);
    procedure WriteRegistro0500(Reg0001: TRegistro0001);
    procedure WriteRegistro0600(Reg0001: TRegistro0001);
    procedure WriteRegistro0900(Reg0001: TRegistro0001);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function Registro0000New: TRegistro0000;
    function Registro0001New: TRegistro0001;
    function Registro0035New: TRegistro0035;
    function Registro0100New: TRegistro0100;
    function Registro0110New: TRegistro0110;
    function Registro0111New: TRegistro0111;
    function Registro0120New: TRegistro0120;  //Adicionado por Fábio Gabriel - 29/11/2012
    function Registro0140New: TRegistro0140;
    function Registro0145New: TRegistro0145;
    function Registro0150New: TRegistro0150;
    function Registro0190New: TRegistro0190;
    function Registro0200New: TRegistro0200;
    function Registro0205New: TRegistro0205;
    function Registro0206New: TRegistro0206;
    function Registro0208New: TRegistro0208;
    function Registro0400New: TRegistro0400;
    function Registro0450New: TRegistro0450;
    function Registro0500New: TRegistro0500;
    function Registro0600New: TRegistro0600;
    function Registro0900New: TRegistro0900;

    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;

    property Registro0000: TRegistro0000 read FRegistro0000 write FRegistro0000;
    property Registro0001: TRegistro0001 read FRegistro0001 write FRegistro0001;
    property Registro0990: TRegistro0990 read FRegistro0990 write FRegistro0990;

    property Registro0035Count: Integer read FRegistro0035Count write FRegistro0035Count;
    property Registro0100Count: Integer read FRegistro0100Count write FRegistro0100Count;
    property Registro0110Count: Integer read FRegistro0110Count write FRegistro0110Count;
    property Registro0111Count: Integer read FRegistro0111Count write FRegistro0111Count;
    property Registro0120Count: Integer read FRegistro0120Count write FRegistro0120Count;  //Adicionado por Fábio Gabriel - 29/11/2012
    property Registro0140Count: Integer read FRegistro0140Count write FRegistro0140Count;
    property Registro0145Count: Integer read FRegistro0145Count write FRegistro0145Count;
    property Registro0150Count: Integer read FRegistro0150Count write FRegistro0150Count;
    property Registro0190Count: Integer read FRegistro0190Count write FRegistro0190Count;
    property Registro0200Count: Integer read FRegistro0200Count write FRegistro0200Count;
    property Registro0205Count: Integer read FRegistro0205Count write FRegistro0205Count;
    property Registro0206Count: Integer read FRegistro0206Count write FRegistro0206Count;
    property Registro0208Count: Integer read FRegistro0208Count write FRegistro0208Count;
    property Registro0400Count: Integer read FRegistro0400Count write FRegistro0400Count;
    property Registro0450Count: Integer read FRegistro0450Count write FRegistro0450Count;
    property Registro0500Count: Integer read FRegistro0500Count write FRegistro0500Count;
    property Registro0600Count: Integer read FRegistro0600Count write FRegistro0600Count;
  end;

implementation

uses ACBrTXTUtils, StrUtils;

{ TBloco_0 }

constructor TBloco_0.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_0.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_0.CriaRegistros;
begin
  FRegistro0000 := TRegistro0000.Create;
  FRegistro0001 := TRegistro0001.Create;
  FRegistro0990 := TRegistro0990.Create;

  FRegistro0035Count := 0;
  FRegistro0100Count := 0;
  FRegistro0110Count := 0;
  FRegistro0111Count := 0;
  FRegistro0120Count := 0;  //Adicionado por Fábio Gabriel - 29/11/2012
  FRegistro0140Count := 0;
  FRegistro0145Count := 0;
  FRegistro0150Count := 0;
  FRegistro0190Count := 0;
  FRegistro0200Count := 0;
  FRegistro0205Count := 0;
  FRegistro0206Count := 0;
  FRegistro0208Count := 0;
  FRegistro0400Count := 0;
  FRegistro0450Count := 0;
  FRegistro0500Count := 0;
  FRegistro0600Count := 0;

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

function TBloco_0.Registro0000New: TRegistro0000;
begin
   Result := FRegistro0000;
end;

function TBloco_0.Registro0001New: TRegistro0001;
begin
   Result := FRegistro0001;
end;

function TBloco_0.Registro0035New: TRegistro0035;
begin
  Result := FRegistro0001.Registro0035.New;
end;

function TBloco_0.Registro0100New: TRegistro0100;
begin
   Result := FRegistro0001.Registro0100.New;
end;

function TBloco_0.Registro0110New: TRegistro0110;
begin
   Result := FRegistro0001.Registro0110;
end;

function TBloco_0.Registro0111New: TRegistro0111;
begin
   Result := FRegistro0001.Registro0110.Registro0111;
end;

// Adicionado por Fábio Gabriel - 29/11/2012
function TBloco_0.Registro0120New: TRegistro0120;
begin
   Result := FRegistro0001.Registro0120.New;
end;

function TBloco_0.Registro0140New: TRegistro0140;
begin
   Result := FRegistro0001.Registro0140.New();
end;

function TBloco_0.Registro0150New: TRegistro0150;
var
U0140: TRegistro0140;
U0140Count: Integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   if U0140Count = -1 then
      raise Exception.Create('O registro 0150 deve ser filho do registro 0140, e não existe nenhum 0140 pai!');
   //
   U0140  := FRegistro0001.Registro0140.Items[U0140Count];
   Result := U0140.Registro0150.New;
end;

function TBloco_0.Registro0190New: TRegistro0190;
var
U0140: TRegistro0140;
U0140Count: Integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   if U0140Count = -1 then
      raise Exception.Create('O registro 0190 deve ser filho do registro 0140, e não existe nenhum 0140 pai!');
   //
   U0140  := FRegistro0001.Registro0140.Items[U0140Count];
   Result := U0140.Registro0190.New;
end;

function TBloco_0.Registro0200New: TRegistro0200;
var
U0140: TRegistro0140;
U0140Count: Integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   if U0140Count = -1 then
      raise Exception.Create('O registro 0200 deve ser filho do registro 0140, e não existe nenhum 0140 pai!');
   //
   U0140  := FRegistro0001.Registro0140.Items[U0140Count];
   Result := U0140.Registro0200.New();
end;

function TBloco_0.Registro0205New: TRegistro0205;
var
U0200: TRegistro0200;
U0140Count: integer;
U0200Count: integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   U0200Count := FRegistro0001.Registro0140.Items[U0140Count].Registro0200.Count -1;
   if U0200Count = -1 then
      raise Exception.Create('O registro 0205 deve ser filho do registro 0200, e não existe nenhum 0200 pai!');
   //
   U0200  := FRegistro0001.Registro0140.Items[U0140Count].Registro0200.Items[U0200Count];
   Result := U0200.Registro0205.New;
end;

function TBloco_0.Registro0206New: TRegistro0206;
var
U0140Count: integer;
U0200Count: integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   U0200Count := FRegistro0001.Registro0140.Items[U0140Count].Registro0200.Count -1;
   //
   Result := FRegistro0001.Registro0140.Items[U0140Count].Registro0200.Items[U0200Count].Registro0206;
end;

function TBloco_0.Registro0208New: TRegistro0208;
var
U0140Count: integer;
U0200Count: integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   U0200Count := FRegistro0001.Registro0140.Items[U0140Count].Registro0200.Count -1;
   //
   Result := FRegistro0001.Registro0140.Items[U0140Count].Registro0200.Items[U0200Count].Registro0208;
end;

function TBloco_0.Registro0400New: TRegistro0400;
var
U0140: TRegistro0140;
U0140Count: Integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   if U0140Count = -1 then
      raise Exception.Create('O registro 0150 deve ser filho do registro 0140, e não existe nenhum 0140 pai!');
   //
   U0140  := FRegistro0001.Registro0140.Items[U0140Count];
   Result := U0140.Registro0400.New;
end;

function TBloco_0.Registro0450New: TRegistro0450;
var
U0140: TRegistro0140;
U0140Count: Integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   if U0140Count = -1 then
      raise Exception.Create('O registro 0150 deve ser filho do registro 0140, e não existe nenhum 0140 pai!');
   //
   U0140  := FRegistro0001.Registro0140.Items[U0140Count];
   Result := U0140.Registro0450.New;
end;

function TBloco_0.Registro0500New: TRegistro0500;
begin
   Result := FRegistro0001.Registro0500.New;
end;

function TBloco_0.Registro0600New: TRegistro0600;
begin
   Result := FRegistro0001.Registro0600.New;
end;

function TBloco_0.Registro0900New: TRegistro0900;
begin
  if not assigned(FRegistro0001.Registro0900) then
  begin
    FRegistro0001.Registro0900 := TRegistro0900.Create;
  end;
  Result := FRegistro0001.Registro0900;
end;

procedure TBloco_0.WriteRegistro0000;
var
strTIPO_ESCRIT: String;
strIND_SIT_ESP: String;
strIND_NAT_PJ: String;
strIND_ATIV: String;
strNUM_REC_ANTERIOR: String;
begin
  if Assigned(Registro0000) then
  begin
     with Registro0000 do
     begin
       case TIPO_ESCRIT of
         tpEscrOriginal: strTIPO_ESCRIT := '0';
         tpEscrRetificadora: strTIPO_ESCRIT := '1';
       end;
       case IND_SIT_ESP of
         indSitAbertura: strIND_SIT_ESP := '0';
         indSitCisao: strIND_SIT_ESP := '1';
         indSitFusao: strIND_SIT_ESP := '2';
         indSitIncorporacao: strIND_SIT_ESP := '3';
         indSitEncerramento: strIND_SIT_ESP := '4';
         indNenhum: strIND_SIT_ESP := '';
       end;
       case IND_NAT_PJ of
         indNatPJNenhum               : strIND_NAT_PJ := '';
         indNatPJSocEmpresariaGeral   : strIND_NAT_PJ := '00'; //0 - Sociedade empresária geral
         indNatPJSocCooperativa       : strIND_NAT_PJ := '01'; //1 - Sociedade Cooperativa
         indNatPJEntExclusivaFolhaSal : strIND_NAT_PJ := '02'; //2 - Entidade sujeita ao PIS/Pasep exclusivamente com base  na folha de salários
         indNatPJSocEmpresariaGeralSCP: strIND_NAT_PJ := '03'; //3 - Sociedade empresária geral SCP
         indNatPJSocCooperativaSCP    : strIND_NAT_PJ := '04'; //4 - Sociedade Cooperativa SCP
         indNatPJSocContaParticante   : strIND_NAT_PJ := '05'; //5 - SCP
       end;
       case IND_ATIV of
         indAtivIndustrial: strIND_ATIV := '0';
         indAtivPrestadorServico: strIND_ATIV := '1';
         indAtivComercio: strIND_ATIV := '2';
         indAtivoFincanceira: strIND_ATIV := '3';
         indAtivoImobiliaria: strIND_ATIV := '4';
         indAtivoOutros: strIND_ATIV := '9';
       end;

       if (TIPO_ESCRIT = tpEscrRetificadora) then
          strNUM_REC_ANTERIOR := NUM_REC_ANTERIOR
       else
          strNUM_REC_ANTERIOR := '';

       Check(funChecaCNPJ(CNPJ), '(0-0000) ENTIDADE: O CNPJ "%s" digitado é inválido!', [CNPJ]);
       Check(funChecaUF(UF), '(0-0000) ENTIDADE: A UF "%s" digitada é inválido!', [UF]);
       Check(funChecaMUN(COD_MUN), '(0-0000) ENTIDADE: O código do município "%s" digitado é inválido!', [IntToStr(COD_MUN)]);
       ///
       Add( LFill( '0000' ) +
            LFill( CodVerToStr(COD_VER) ) +
            LFill( strTIPO_ESCRIT ) +
            LFill( strIND_SIT_ESP ) +
            LFill( strNUM_REC_ANTERIOR) +
            LFill( DT_INI ) +
            LFill( DT_FIN ) +
            LFill( NOME ) +
            LFill( CNPJ ) +
            LFill( UF ) +
            LFill( COD_MUN, 7 ) +
            LFill( SUFRAMA, 9, True ) +
            LFill( strIND_NAT_PJ ) +
            LFill( strIND_ATIV ) );
       ///
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0001 ;
begin
  if Assigned(FRegistro0001) then
  begin
     with FRegistro0001 do
     begin
        Add( LFill( '0001' ) +
             LFill( Integer(IND_MOV), 0 ) ) ;

        if IND_MOV = imComDados then
        begin
          WriteRegistro0035(FRegistro0001) ;
          WriteRegistro0100(FRegistro0001) ;
          WriteRegistro0110(FRegistro0001) ;
          WriteRegistro0120(FRegistro0001) ;   //Implementado por Fábio Gabriel
          WriteRegistro0140(FRegistro0001) ;
          WriteRegistro0500(FRegistro0001) ;
          WriteRegistro0600(FRegistro0001) ;
          WriteRegistro0900(FRegistro0001) ;
        end;
     end;
     Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
  end;
end;

procedure TBloco_0.WriteRegistro0035(Reg0001: TRegistro0001);
var
  intFor: Integer;
begin
  if Self.DT_INI >= EncodeDate(2014,03,01) then
    if Assigned(Reg0001.Registro0035) then
    begin
       for intFor := 0 to Reg0001.Registro0035.Count - 1 do
       begin
          with Reg0001.Registro0035.Items[intFor] do
          begin
            ///
            Add( LFill('0035')   +
                 LFill(COD_SCP, 14)  +
                 LFill(DESC_SCP) +
                 LFill(INF_COMP) ) ;
          end;
          Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
       end;
       /// Variavél para armazenar a quantidade de registro do tipo.
       FRegistro0035Count := FRegistro0035Count + Reg0001.Registro0035.Count;
    end;
end;

procedure TBloco_0.WriteRegistro0100(Reg0001: TRegistro0001) ;
var
  intFor: Integer;
begin
  if Assigned(Reg0001.Registro0100) then
  begin
     for intFor := 0 to Reg0001.Registro0100.Count - 1 do
     begin
        with Reg0001.Registro0100.Items[intFor] do
        begin
          //VERIFICA SE NÃO SÃO OS DOIS VAZIOS, SE SÃO MOSTRA MENSAGEM DE QUE PELO MENOS 1
          //É OBRIGATÓRIO
          if (Trim(CPF) = '') and (Trim(CNPJ) = '') then
            Check(False, '(0-0100) CONTADOR %s: O CNPJ/CPF é obrigatório!', [NOME]);

          //CRC É DE PREENCHIMENTO OBRIGATÓRIO DE ACORDO COM O VALIDADOR
          Check(CRC <> '', '(0-0100) CONTADOR %s: O CRC é obrigatório!', [NOME]);

          Check(funChecaCPF(CPF),     '(0-0100) CONTADOR %s: O CPF "%s" digitado é inválido!', [NOME, CPF]);
          Check(funChecaCNPJ(CNPJ),   '(0-0100) CONTADOR %s: O CNPJ "%s" digitado é inválido!', [NOME, CNPJ]);
          //Check(funChecaCEP(CEP, Registro0000.UF), '(0-0100) CONTADOR: %s, o CEP "%s" digitada é inválido para a unidade de federação "%s"!', [NOME, CEP, Registro0000.UF]);
          Check(funChecaMUN(COD_MUN), '(0-0100) CONTADOR %s: O código do município "%s" digitado é inválido!', [NOME, IntToStr(COD_MUN)]);
          Check(NOME <> '', '(0-0100) CONTADOR: O nome do contabilista/escritório é obrigatório!');
          ///
          Add( LFill('0100') +
               LFill(NOME) +
               LFill(CPF) +
               LFill(CRC) +
               LFill(CNPJ) +
               LFill(CEP, 8) +
               LFill(ENDERECO) +
               LFill(NUM) +
               LFill(COMPL) +
               LFill(BAIRRO) +
               LFill(FONE, 10) +
               LFill(FAX, 10, True) +
               LFill(EMAIL) +
               LFill(COD_MUN, 7) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0100Count := FRegistro0100Count + Reg0001.Registro0100.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0110(Reg0001: TRegistro0001) ;
var
strCOD_INC_TRIB: string;
strIND_APRO_CRED: string;
strCOD_TIPO_CONT: string;
strIND_REG_CUM: string;
begin
  if Assigned(Reg0001.Registro0110) then
  begin
     with Reg0001.Registro0110 do
     begin
       case COD_INC_TRIB of
         codEscrOpIncNaoCumulativo: strCOD_INC_TRIB := '1';
         codEscrOpIncCumulativo: strCOD_INC_TRIB := '2';
         codEscrOpIncAmbos: strCOD_INC_TRIB := '3';
       end;

       case IND_APRO_CRED of
         indMetodoApropriacaoDireta: strIND_APRO_CRED := '1';
         indMetodoDeRateioProporcional: strIND_APRO_CRED := '2';
       end;

       case COD_TIPO_CONT of
         codIndTipoConExclAliqBasica: strCOD_TIPO_CONT := '1';
         codIndTipoAliqEspecificas: strCOD_TIPO_CONT := '2';
       end;

       case IND_REG_CUM of
         codRegimeCaixa : strIND_REG_CUM := '1';
         codRegimeCompetEscritConsolidada : strIND_REG_CUM := '2';
         codRegimeCompetEscritDetalhada : strIND_REG_CUM := '9';
       end;

       ///
       if FRegistro0000.COD_VER >= vlVersao201 then
       begin
         if (COD_INC_TRIB = codEscrOpIncCumulativo) then
           strIND_APRO_CRED := '';// Conforme Guia prático 1.0.5 Deve ser vazio caso COD_INC_TRIB = 2

         if (COD_INC_TRIB <> codEscrOpIncCumulativo) then // Mário Mesquita -- Conforme guia prático, Deve ser vazio caso COD_INC_TRIB <> 2
           strIND_REG_CUM := '';
         if Self.FRegistro0000.IND_ATIV=indAtivoFincanceira then 
           strIND_REG_CUM := '';

         //Nota: Só a versão 2.01 ou superior do PVA vai estar pronta para validar esse arquivo.
         Add( LFill('0110') +
              LFill( strCOD_INC_TRIB  ) +
              LFill( strIND_APRO_CRED ) +
              LFill( strCOD_TIPO_CONT ) +
              LFill( strIND_REG_CUM ) );
       end
       else if FRegistro0000.COD_VER >= vlVersao101 then
       begin

         //Verificar a necessidade desse if abaixo quando sair a versão 2.0 do PVA PisCofins
         if (COD_INC_TRIB = codEscrOpIncCumulativo) then
           strIND_APRO_CRED := '';// Conforme Guia prático 1.0.5 Deve ser vazio caso COD_INC_TRIB = 2
         Add( LFill('0110') +
              LFill( strCOD_INC_TRIB  ) +
              LFill( strIND_APRO_CRED ) +
              LFill( strCOD_TIPO_CONT ) );
       end
       else //Modelos de registro anteriores à versao 1.0.3 do guia prático
         Add( LFill('0110') +
              LFill( strCOD_INC_TRIB ) +
              LFill( strIND_APRO_CRED ) +
              LFill( strCOD_TIPO_CONT ) ) ;
       ///
       // O (se no registro 0110 o Campo “COD_INC_TRIB” = 1 ou 3 e o Campo “IND_APRO_CRED” = 2)
       // N (se no registro 0110 o Campo “COD_INC_TRIB” = 2 ou     o Campo “IND_APRO_CRED” = 1)
       if (COD_INC_TRIB = codEscrOpIncNaoCumulativo) or (COD_INC_TRIB = codEscrOpIncAmbos) then
       begin
          if IND_APRO_CRED = indMetodoDeRateioProporcional then
             WriteRegistro0111(Reg0001.Registro0110);
       end;
     end;
     Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;

     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0110Count := FRegistro0110Count + 1;
  end;
end;

procedure TBloco_0.WriteRegistro0111(Reg0110: TRegistro0110) ;
begin
  if Assigned(Reg0110.Registro0111) then
  begin
     with Reg0110.Registro0111 do
     begin
       Add( LFill('0111') +
            LFill( REC_BRU_NCUM_TRIB_MI, 0, 2 ) +
            LFill( REC_BRU_NCUM_NT_MI, 0, 2 ) +
            LFill( REC_BRU_NCUM_EXP, 0, 2 ) +
            LFill( REC_BRU_CUM, 0, 2 ) +
            LFill( REC_BRU_TOTAL, 0, 2 ) ) ;
     end;
     Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;

     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0111Count := FRegistro0111Count + 1;
  end;
end;

// Adicionado por Fábio Gabriel - 29/11/2012
procedure TBloco_0.WriteRegistro0120(Reg0001: TRegistro0001) ;
var
intFor: Integer;
begin
  if Assigned(Reg0001.Registro0120) then
  begin
     for intFor := 0 to Reg0001.Registro0120.Count - 1 do
     begin
        with Reg0001.Registro0120.Items[intFor] do
        begin
           Add( LFill('0120') +
                LFill( MES_DISPENSA, 6 ) +  //Implementado //Formato MMAAAA
                LFill( INF_COMP ) ) ;       //Implementado
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0120Count := FRegistro0120Count + Reg0001.Registro0120.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0140(Reg0001: TRegistro0001) ;
var
intFor: Integer;
begin
  if Assigned(Reg0001.Registro0140) then
  begin
     for intFor := 0 to Reg0001.Registro0140.Count - 1 do
     begin
        with Reg0001.Registro0140.Items[intFor] do
        begin
           Add( LFill('0140') +
                LFill( COD_EST ) +
                LFill( NOME ) +
                LFill( CNPJ ) +
                LFill( UF ) +
                LFill( IE ) +
                LFill( COD_MUN, 7 ) +
                LFill( IM ) +
                LFill( SUFRAMA ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistro0145( Reg0001.Registro0140.Items[intFor] ) ;
        WriteRegistro0150( Reg0001.Registro0140.Items[intFor] ) ;
        WriteRegistro0190( Reg0001.Registro0140.Items[intFor] ) ;
        WriteRegistro0200( Reg0001.Registro0140.Items[intFor] ) ;
        WriteRegistro0400( Reg0001.Registro0140.Items[intFor] ) ;
        WriteRegistro0450( Reg0001.Registro0140.Items[intFor] ) ;

        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0140Count := FRegistro0140Count + Reg0001.Registro0140.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0150(Reg0140: TRegistro0140) ;
var
  intFor: integer;
  booExterior: Boolean;
begin
  if Assigned(Reg0140.Registro0150) then
  begin
     for intFor := 0 to Reg0140.Registro0150.Count - 1 do
     begin
        with Reg0140.Registro0150.Items[intFor] do
        begin
		  booExterior := ((COD_PAIS <> '01058') and (COD_PAIS <> '1058'));
//          Check(funChecaPAISIBGE(COD_PAIS), '(0-0150) %s-%s, o código do país "%s" digitado é inválido!', [COD_PART, NOME, COD_PAIS]);
          if Length(CNPJ) > 0 then Check(funChecaCNPJ(CNPJ), '(0-0150) %s-%s, o CNPJ "%s" digitado é inválido!', [COD_PART, NOME, CNPJ]);
          if Length(CPF)  > 0 then Check(funChecaCPF(CPF), '(0-0150) %s-%s, o CPF "%s" digitado é inválido!', [COD_PART, NOME, CPF]);
//          Check(funChecaIE(IE, UF),         '(0-0150) %s-%s, a Inscrição Estadual "%s" digitada é inválida!', [COD_PART, NOME, IE]);
//          Check(funChecaMUN(COD_MUN),       '(0-0150) %s-%s, o código do município "%s" digitado é inválido!', [COD_PART, NOME, IntToStr(COD_MUN)]);
          Check(NOME <> '',                 '(0-0150) O nome do participante é obrigatório!');
          ///
          Add( LFill('0150') +
               LFill(COD_PART) +
               LFill(NOME) +
               LFill(COD_PAIS) +
               LFill(CNPJ) +
               LFill(CPF) +
               LFill(IE) +
               IfThen(booExterior, LFill(''), LFill(COD_MUN, 7)) +
               LFill(SUFRAMA) +
               LFill(ENDERECO) +
               LFill(NUM) +
               LFill(COMPL) +
               LFill(BAIRRO) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0150Count := FRegistro0150Count + Reg0140.Registro0150.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0190(Reg0140: TRegistro0140) ;
var
intFor: integer;
begin
  if Assigned(Reg0140.Registro0190) then
  begin
     for intFor := 0 to Reg0140.Registro0190.Count - 1 do
     begin
        with Reg0140.Registro0190.Items[intFor] do
        begin

          Add( LFill('0190') +
               LFill(UNID) +
               LFill(DESCR) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0190Count := FRegistro0190Count + Reg0140.Registro0190.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0200(Reg0140: TRegistro0140) ;
var
  intFor: integer;
  strTIPO_ITEM: String;
begin
  if Assigned( Reg0140.Registro0200 ) then
  begin
     for intFor := 0 to Reg0140.Registro0200.Count - 1 do
     begin
        with Reg0140.Registro0200.Items[intFor] do
        begin
          case TIPO_ITEM of
            tiMercadoriaRevenda    : strTIPO_ITEM := '00';
            tiMateriaPrima         : strTIPO_ITEM := '01';
            tiEmbalagem            : strTIPO_ITEM := '02';
            tiProdutoProcesso      : strTIPO_ITEM := '03';
            tiProdutoAcabado       : strTIPO_ITEM := '04';
            tiSubproduto           : strTIPO_ITEM := '05';
            tiProdutoIntermediario : strTIPO_ITEM := '06';
            tiMaterialConsumo      : strTIPO_ITEM := '07';
            tiAtivoImobilizado     : strTIPO_ITEM := '08';
            tiServicos             : strTIPO_ITEM := '09';
            tiOutrosInsumos        : strTIPO_ITEM := '10';
            tiOutras               : strTIPO_ITEM := '99';
          end;
          if Length(COD_GEN) > 0 then
             Check(funChecaGENERO(COD_GEN), '(0-0200) O código do gênero "%s" digitado é inválido!', [COD_GEN]);
          ///
          Add( LFill('0200') +
               LFill( COD_ITEM ) +
               LFill( DESCR_ITEM ) +
               LFill( COD_BARRA ) +
               LFill( COD_ANT_ITEM ) +
               LFill( UNID_INV ) +
               LFill( strTIPO_ITEM ) +
               LFill( COD_NCM ) +
               LFill( EX_IPI ) +
               LFill( COD_GEN ) +
               LFill( COD_LST ) +
               VLFill( ALIQ_ICMS, 6, 2 ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistro0205( Reg0140.Registro0200.Items[intFor] ) ;
        WriteRegistro0206( Reg0140.Registro0200.Items[intFor] ) ;
        WriteRegistro0208( Reg0140.Registro0200.Items[intFor] );

        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0200Count := FRegistro0200Count + Reg0140.Registro0200.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0205(Reg0200: TRegistro0200) ;
var
  intFor: integer;
begin
  if Assigned( Reg0200.Registro0205 ) then
  begin
     for intFor := 0 to Reg0200.Registro0205.Count - 1 do
     begin
        with Reg0200.Registro0205.Items[intFor] do
        begin
          Add( LFill('0205') +
               LFill( DESCR_ANT_ITEM ) +
               LFill( DT_INI ) +
               LFill( DT_FIM ) +
               LFill( COD_ANT_ITEM) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0205Count := FRegistro0205Count + Reg0200.Registro0205.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0206(Reg0200: TRegistro0200) ;
begin
  if Assigned( Reg0200.Registro0206 ) then
  begin
     if Trim(Reg0200.Registro0206.COD_COMB) <> '' then
     begin
        with Reg0200.Registro0206 do
        begin
           Add( LFill('0206') +
                LFill( COD_COMB ) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;

        /// Variavél para armazenar a quantidade de registro do tipo.
        FRegistro0206Count := FRegistro0206Count + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0208(Reg0200: TRegistro0200) ;
var
strCOD_TAB: string;
begin
  if Assigned( Reg0200.Registro0208 ) then
  begin
     if Reg0200.Registro0208.COD_TAB <> codIndiTabNaoTem then
     begin
        with Reg0200.Registro0208 do
        begin
          case COD_TAB of
             codIndTabI: strCOD_TAB := '01';
             codIndTabII: strCOD_TAB := '02';
             codIndTabIII: strCOD_TAB := '03';
             codIndTabIV: strCOD_TAB := '04';
             codIndTabV: strCOD_TAB := '05';
             codIndTabVI: strCOD_TAB := '06';
             codIndTabVII: strCOD_TAB := '07';
             codIndTabVIII: strCOD_TAB := '08';
             codIndTabIX: strCOD_TAB := '09';
             codIndTabX: strCOD_TAB := '10';
             codIndTabXI: strCOD_TAB := '11';
             codIndiTabXII: strCOD_TAB := '12';
          end;
          Add( LFill('0208') +
               LFill( strCOD_TAB ) +
               LFill( COD_GRU ) +
               LFill( MARCA_COM ) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;

        /// Variavél para armazenar a quantidade de registro do tipo.
        FRegistro0208Count := FRegistro0208Count + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0400(Reg0140: TRegistro0140) ;
var
  intFor: integer;
begin
  if Assigned(Reg0140.Registro0400) then
  begin
     for intFor := 0 to Reg0140.Registro0400.Count - 1 do
     begin
        with Reg0140.Registro0400.Items[intFor] do
        begin
          ///
          Add( LFill('0400') +
               LFill( COD_NAT ) +
               LFill( DESCR_NAT ) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0400Count := FRegistro0400Count + Reg0140.Registro0400.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0450(Reg0140: TRegistro0140) ;
var
  intFor: integer;
begin
  if Assigned( Reg0140.Registro0450 ) then
  begin
     for intFor := 0 to Reg0140.Registro0450.Count - 1 do
     begin
        with Reg0140.Registro0450.Items[intFor] do
        begin
          Add( LFill('0450') +
               LFill( COD_INF ) +
               LFill( TXT ) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0450Count := FRegistro0450Count + Reg0140.Registro0450.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0500(Reg0001: TRegistro0001) ;
var
intFor: integer;
strCOD_NAT_CC: string;
strIND_CTA: string;
begin
  if Assigned( Reg0001.Registro0500 ) then
  begin
     for intFor := 0 to Reg0001.Registro0500.Count - 1 do
     begin
        with Reg0001.Registro0500.Items[intFor] do
        begin
           case COD_NAT_CC of
             ncgAtivo: strCOD_NAT_CC := '01';
             ncgPassivo: strCOD_NAT_CC := '02';
             ncgLiquido: strCOD_NAT_CC := '03';
             ncgResultado: strCOD_NAT_CC := '04';
             ncgCompensacao: strCOD_NAT_CC := '05';
             ncgOutras: strCOD_NAT_CC := '09';
           end;
           case IND_CTA of
             indCTASintetica: strIND_CTA := 'S';
             IndCTAnalitica: strIND_CTA := 'A';
           end;

           Check(Pos(strCOD_NAT_CC, '01,02,03,04,05,09,10,99') > 0, '(0-0500) O código da natureza da conta/grupo de contas "%s" digitado é inválido!', [strCOD_NAT_CC]);
           Check(((strIND_CTA = 'S') or (strIND_CTA = 'A')), '(0-0500) O indicador "%s" do tipo de conta, deve ser informado  S ou A!', [strIND_CTA]);

           Add( LFill('0500') +
                LFill( DT_ALT ) +
                LFill( strCOD_NAT_CC ) +
                LFill( strIND_CTA, 1) +
                LFill( NIVEL ) +
                LFill( COD_CTA ) +
                LFill( NOME_CTA ) +
                LFill( COD_CTA_REF ) +
                LFill( CNPJ_EST ) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0500Count := FRegistro0500Count + Reg0001.Registro0500.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0600(Reg0001: TRegistro0001) ;
var
  intFor: integer;
begin
  if Assigned( Reg0001.Registro0600 ) then
  begin
     for intFor := 0 to Reg0001.Registro0600.Count - 1 do
     begin
        with Reg0001.Registro0600.Items[intFor] do
        begin
           Add( LFill('0600') +
                LFill( DT_ALT ) +
                LFill( COD_CCUS ) +
                LFill( CCUS ) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0600Count := FRegistro0600Count + Reg0001.Registro0600.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0900(Reg0001: TRegistro0001);
begin
  if Assigned(Reg0001.Registro0900) then
  begin
     with Reg0001.Registro0900 do
     begin
       ///
       Add(
         {01} LFill('0990') +
         {02} VDFill( REC_TOTAL_BLOCO_A    , 2) +
         {03} VDFill( REC_NRB_BLOCO_A      , 2) +
         {04} VDFill( REC_TOTAL_BLOCO_C    , 2) +
         {05} VDFill( REC_NRB_BLOCO_C      , 2) +
         {06} VDFill( REC_TOTAL_BLOCO_D    , 2) +
         {07} VDFill( REC_NRB_BLOCO_D      , 2) +
         {08} VDFill( REC_TOTAL_BLOCO_F    , 2) +
         {09} VDFill( REC_NRB_BLOCO_F      , 2) +
         {10} VDFill( REC_TOTAL_BLOCO_I    , 2) +
         {11} VDFill( REC_NRB_BLOCO_I      , 2) +
         {12} VDFill( REC_TOTAL_BLOCO_1    , 2) +
         {13} VDFill( REC_NRB_BLOCO_1      , 2) +
         {14} VDFill( REC_TOTAL_PERIODO    , 2) +
         {15} VDFill( REC_TOTAL_NRB_PERIODO, 2)
         );
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end
end;

procedure TBloco_0.WriteRegistro0990 ;
begin
  if Assigned(Registro0990) then
  begin
     with Registro0990 do
     begin
       QTD_LIN_0 := QTD_LIN_0 + 1;
       ///
       Add( LFill('0990') +
            LFill(QTD_LIN_0,0) );
     end;
  end;
end;

function TBloco_0.Registro0145New: TRegistro0145;
var
U0140Count: Integer;
begin
   U0140Count := FRegistro0001.Registro0140.Count -1;
   //
   Result := FRegistro0001.Registro0140.Items[U0140Count].Registro0145;
end;

procedure TBloco_0.WriteRegistro0145(Reg0140: TRegistro0140);
begin
  if Assigned(Reg0140.Registro0145) and (Reg0140.Registro0145.COD_INC_TRIB <> '') then
  begin
    with Reg0140.Registro0145 do
    begin
      Add( LFill('0145')                 +
           LFill(COD_INC_TRIB)           +
           LFill(VL_REC_TOT,0,2)         +
           LFill(VL_REC_ATIV,0,2)        +
           LFill(VL_REC_DEMAIS_ATIV,0,2) +
           LFill(INFO_COMPL));
    end;
    Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;

    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro0145Count := FRegistro0145Count + 1;
  end;
end;

end.
