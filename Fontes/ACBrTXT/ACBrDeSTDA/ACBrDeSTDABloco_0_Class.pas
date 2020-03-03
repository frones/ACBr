{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: João Pedro R Costa                              }
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

unit ACBrDeSTDABloco_0_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrDeSTDA4715, ACBrDeSTDABlocos, ACBrDeSTDABloco_0,
     ACBrTXTClass;


type
  /// TBLOCO_0 - Abertura, Identificação e Referências

  { TBloco_0 }

  TBloco_0 = class(TACBrDeSTDA4715)
  private
//    FOnBeforeWriteRegistro0200: TWriteRegistroEvent;
    FOnBeforeWriteRegistro0990: TWriteRegistroEvent;

//    FOnWriteRegistro0200: TWriteRegistroEvent;
    FOnWriteRegistro0990: TWriteRegistroEvent;

//    FOnAfterWriteRegistro0200: TWriteRegistroEvent;
    FOnAfterWriteRegistro0990: TWriteRegistroEvent;

    FRegistro0000: TRegistro0000;      /// BLOCO 0 - Registro0000
    FRegistro0001: TRegistro0001;      /// BLOCO 0 - Registro0001
    FRegistro0990: TRegistro0990;      /// BLOCO 0 - Registro0990

    FRegistro0002Count: Integer;

    procedure WriteRegistro0002(Reg0001: TRegistro0001);
    procedure WriteRegistro0005(Reg0001: TRegistro0001);
    procedure WriteRegistro0030(Reg0001: TRegistro0001);
    procedure WriteRegistro0100(Reg0001: TRegistro0001);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function Registro0000New: TRegistro0000;
    function Registro0001New: TRegistro0001;
    function Registro0002New: TRegistro0002;
    function Registro0005New: TRegistro0005;
    function Registro0030New: TRegistro0030;
    function Registro0100New: TRegistro0100;

    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;

    property Registro0000: TRegistro0000 read FRegistro0000 write FRegistro0000;
    property Registro0001: TRegistro0001 read FRegistro0001 write FRegistro0001;
    property Registro0990: TRegistro0990 read FRegistro0990 write FRegistro0990;

    property Registro0002Count: Integer read FRegistro0002Count write FRegistro0002Count;

//    property OnBeforeWriteRegistro0200: TWriteRegistroEvent read FOnBeforeWriteRegistro0200 write FOnBeforeWriteRegistro0200;
    property OnBeforeWriteRegistro0990: TWriteRegistroEvent read FOnBeforeWriteRegistro0990 write FOnBeforeWriteRegistro0990;

//    property OnWriteRegistro0200      : TWriteRegistroEvent read FOnWriteRegistro0200       write FOnWriteRegistro0200;
    property OnWriteRegistro0990      : TWriteRegistroEvent read FOnWriteRegistro0990       write FOnWriteRegistro0990;

//    property OnAfterWriteRegistro0200 : TWriteRegistroEvent read FOnAfterWriteRegistro0200  write FOnAfterWriteRegistro0200;
    property OnAfterWriteRegistro0990 : TWriteRegistroEvent read FOnAfterWriteRegistro0990  write FOnAfterWriteRegistro0990;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_0 }

constructor TBloco_0.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TBloco_0.CriaRegistros;
begin
  FRegistro0000 := TRegistro0000.Create;
  FRegistro0001 := TRegistro0001.Create;
  FRegistro0990 := TRegistro0990.Create;

  FRegistro0002Count := 0;

  FRegistro0990.QTD_LIN_0 := 0;
end;

destructor TBloco_0.Destroy;
begin
  LiberaRegistros;
  inherited;
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

function TBloco_0.Registro0002New: TRegistro0002;
begin
   Result := FRegistro0001.Registro0002.New(FRegistro0001);
end;

function TBloco_0.Registro0005New: TRegistro0005;
begin
   Result := FRegistro0001.Registro0005;
end;

function TBloco_0.Registro0030New: TRegistro0030;
begin
   Result := FRegistro0001.Registro0030;
end;

function TBloco_0.Registro0100New: TRegistro0100;
begin
   Result := FRegistro0001.Registro0100;
end;

procedure TBloco_0.WriteRegistro0000;
var
  strCOD_VER: string;
  strCOD_FIN: string;
  strCOD_CTD: string;
begin
  if Assigned(Registro0000) then
  begin
     with Registro0000 do
     begin

       Check(funChecaCNPJ(CNPJ), '(0-0000) ENTIDADE: O CNPJ "%s" digitado é inválido!', [CNPJ]);
       Check(funChecaCPF(CPF), '(0-0000) ENTIDADE: O CPF "%s" digitado é inválido!', [CPF]);
       Check(funChecaUF(UF), '(0-0000) ENTIDADE: A UF "%s" digitada é inválido!', [UF]);
       Check(funChecaIE(IE, UF), '(0-0000) ENTIDADE: A inscrição estadual "%s" digitada é inválida!', [IE]);
       Check(funChecaMUN(COD_MUN), '(0-0000) ENTIDADE: O código do município "%s" digitado é inválido!', [IntToStr(COD_MUN)]);

       case COD_VER of
         vlVersao2000: strCOD_VER := '2000';
         vlVersao2001: strCOD_VER := '2001';
         vlVersao2010: strCOD_VER := '2010';
         vlVersao2100: strCOD_VER := '2100';
       end;

       case COD_FIN of
         raOriginal  : strCOD_FIN := '0' ;
         raSubstituto: strCOD_FIN := '1';
       end;

       case COD_CTD of
         cn30   : strCOD_CTD := '30';
         cnNullo: strCOD_CTD := '';
       end;

       ///
       Add( LFill( '0000' ) +
            LFill( 'LFPD' ) +
            LFill( DT_INI, 'ddmmyyyy' ) +
            LFill( DT_FIN, 'ddmmyyyy' ) +
            LFill( NOME_EMPR ) +
            LFill( CNPJ ) +
            LFill( UF ) +
            LFill( IE ) +
            LFill( COD_MUN, 7 ) +
            LFill( IM ) +
            LFill( VAZIO1 ) +
            LFill( SUFRAMA ) +
            LFill( strCOD_VER ) +
            LFill( strCOD_FIN ) +
            LFill( strCOD_CTD ) +
            LFill( PAIS ) +
            LFill( FANTASIA ) +
            LFill( NIRE ) +
            LFill( CPF ) +
            LFill( VAZIO2 )  ) ;
       ///
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0001;
begin
  if Assigned(FRegistro0001) then
  begin
     with FRegistro0001 do
     begin
        Add( LFill( '0001' ) +
             LFill( Integer(IND_MOV), 0 ) ) ;

        if IND_MOV = imComDados then
        begin
          WriteRegistro0002(FRegistro0001) ;
          WriteRegistro0005(FRegistro0001) ;
          WriteRegistro0030(FRegistro0001) ;
          WriteRegistro0100(FRegistro0001) ;
        end;
     end;

     Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
  end;
end;

procedure TBloco_0.WriteRegistro0002(Reg0001: TRegistro0001);
var
  intFor: integer;
begin
  if Assigned(Reg0001.Registro0002) then
  begin
     for intFor := 0 to Reg0001.Registro0002.Count - 1 do
     begin
        with Reg0001.Registro0002.Items[intFor] do
        begin
           Check(funChecaUF(UF),        '(0-0002) CONTRIBUINTE SUBSTITUTO: A UF "%s" digitada é inválido!', [UF]);
           Check(funChecaIE(IE_ST, UF), '(0-0002) CONTRIBUINTE SUBSTITUTO: A Inscrição Estadual "%s" digitada é inválida!', [IE_ST]);
           ///
           Add( LFill('0002') +
                LFill(UF) +
                LFill(IE_ST) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0002Count := FRegistro0002Count + Reg0001.Registro0002.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0005(Reg0001: TRegistro0001);
var
  strCOD_ASSIN: string;
begin
  if Assigned(Reg0001.Registro0005) then
  begin
    with Reg0001.Registro0005 do
    begin
      Check(funChecaCEP(CEP, Registro0000.UF), '(0-0005) COMPLEMENTO DO CONTRIBUITE "%s": O CEP "%s" digitada é inválido para a unidade de federação "%s"!', [NOME_RESP, CEP, Registro0000.UF]);
      Check(funChecaCPF(CPF_RESP),     '(0-0005) COMPLEMENTO DO CONTRIBUINTE: %s, o CPF "%s" digitado é inválido!', [NOME_RESP, CPF_RESP]);
      ///
      strCOD_ASSIN := codAssinanteToStr(COD_ASSIN);

      Add( LFill('0005') +
           LFill(NOME_RESP) +
           LFill(strCOD_ASSIN) +
           LFill(CPF_RESP) +
           LFill(CEP, 8) +
           LFill(ENDR) +
           LFill(NUM) +
           LFill(COMPL) +
           LFill(BAIRRO) +
           LFill(CEP_CP, 8) +
           LFill(CP, 0) +
           LFill(FONE) +
           LFill(FAX) +
           LFill(EMAIL) ) ;
      ///
      Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
//    FRegistro0005Count := FRegistro0005Count + 1;
  end;
end;

procedure TBloco_0.WriteRegistro0030(Reg0001: TRegistro0001);
var
  strIND_ED: string;
  strIND_ARQ: string;
  strPRF_ISS: string;
  strPRF_ICMS: string;
  strPRF_RIDF: string;
  strPRF_RUDF: string;
  strPRF_LMC: string;
  strPRF_RV: string;
  strPRF_RI: string;
  strIND_EC: string;
  strIND_ISS: string;
  strIND_RT: string;
  strIND_ICMS: string;
  strIND_ST: string;
  strIND_AT: string;
  strIND_IPI: string;
  strIND_RI: string;
begin
  if Assigned(Reg0001.Registro0030) then
  begin
    with Reg0001.Registro0030 do
    begin
      case IND_ED of
        edDigitacao         : strIND_ED := '0';
        edImportacaoArquivo : strIND_ED := '1';
        edAdicaoDoc_ArqTexto: strIND_ED := '2';
        edExportArquivo     : strIND_ED := '3';
      end;

      case IND_ARQ of
        dcGuiasInfEconFisc: strIND_ARQ := '7';
      end;

      case PRF_ISS of
        exISSSimplificado: strPRF_ISS := '0';
        exISSIntegral    : strPRF_ISS := '2';
        exISSNaoObrigado : strPRF_ISS := '9';
      end;

      case PRF_ICMS of
        exICMSSimplificado : strPRF_ICMS := '0';
        exICMSIntermediario: strPRF_ICMS := '1';
        exICMSIntegral     : strPRF_ICMS := '2';
        exICMSNaoObrigado  : strPRF_ICMS := '9';
      end;

      case PRF_RIDF of
        snSim: strPRF_RIDF := '0';
        snNao: strPRF_RIDF := '1';
      end;

      case PRF_RUDF of
        snSim: strPRF_RUDF := '0';
        snNao: strPRF_RUDF := '1';
      end;

      case PRF_LMC of
        snSim: strPRF_LMC := '0';
        snNao: strPRF_LMC := '1';
      end;

      case PRF_RV of
        snSim: strPRF_RV := '0';
        snNao: strPRF_RV := '1';
      end;

      case PRF_RI of
        snSim: strPRF_RI := '0';
        snNao: strPRF_RI := '1';
      end;

      case IND_EC of
        ecCompletaEmArquivoDig      : strIND_EC := '1';
        ecCompletaRegistradaPapel   : strIND_EC := '2';
        ecSimplificadaEmArquivoDig  : strIND_EC := '3';
        ecLivroCaixaArquivoDig      : strIND_EC := '4';
        ecLivroCaixaRegistradoPapel : strIND_EC := '5';
        ecNaoObrigado               : strIND_EC := '9';
      end;

      case IND_ISS of
        snSim: strIND_ISS := '0';
        snNao: strIND_ISS := '1';
      end;

      case IND_RT of
        snSim: strIND_RT := '0';
        snNao: strIND_RT := '1';
      end;

      case IND_ICMS of
        snSim: strIND_ICMS := '0';
        snNao: strIND_ICMS := '1';
      end;

      case IND_ST of
        snSim: strIND_ST := '0';
        snNao: strIND_ST := '1';
      end;

      case IND_AT of
        snSim: strIND_AT := '0';
        snNao: strIND_AT := '1';
      end;

      case IND_IPI of
        snSim: strIND_IPI := '0';
        snNao: strIND_IPI := '1';
      end;

      case IND_RI of
        snSim: strIND_RI := '0';
        snNao: strIND_RI := '1';
      end;

      Add( LFill('0030') +
           LFill(strIND_ED) +
           LFill(strIND_ARQ) +
           LFill(strPRF_ISS) +
           LFill(strPRF_ICMS) +
           LFill(strPRF_RIDF) +
           LFill(strPRF_RUDF) +
           LFill(strPRF_LMC) +
           LFill(strPRF_RV) +
           LFill(strPRF_RI) +
           LFill(strIND_EC) +
           LFill(strIND_ISS) +
           LFill(strIND_RT) +
           LFill(strIND_ICMS) +
           LFill(strIND_ST) +
           LFill(strIND_AT) +
           LFill(strIND_IPI) +
           LFill(strIND_RI) ) ;
      ///
      Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
//    FRegistro0030Count := FRegistro0030Count + 1;
  end;
end;

procedure TBloco_0.WriteRegistro0100(Reg0001: TRegistro0001);
var
  strCOD_ASSIN: string;
begin
  if Assigned(Reg0001.Registro0100) then
  begin
     with Reg0001.Registro0100 do
     begin
       Check(funChecaCPF(CPF),     '(0-0100) CONTADOR: %s, o CPF "%s" digitado é inválido!', [NOME, CPF]);
       Check(funChecaCNPJ(CNPJ),   '(0-0100) CONTADOR: %s, o CNPJ "%s" digitado é inválido!', [NOME, CNPJ]);
//       Check(funChecaCEP(CEP, Registro0000.UF), '(0-0100) CONTADOR: %s, o CEP "%s" digitada é inválido para a unidade de federação "%s"!', [NOME, CEP, Registro0000.UF]);
       Check(funChecaMUN(COD_MUN), '(0-0100) CONTADOR: %s, o código do município "%s" digitado é inválido!', [NOME, IntToStr(COD_MUN)]);
       Check(NOME <> '', '(0-0100) CONTADOR: O nome do contabilista/escritório é obrigatório!');
       ///
       strCOD_ASSIN := codAssinanteToStr(COD_ASSIN);

       Add( LFill('0100') +
            LFill(NOME) +
            LFill(strCOD_ASSIN) +
            LFill(CNPJ) +
            LFill(CPF) +
            LFill(CRC) +
            LFill(CEP, 8) +
            LFill(ENDR) +
            LFill(NUM) +
            LFill(COMPL) +
            LFill(BAIRRO) +
            LFill(UF) +
            LFill(COD_MUN, 7) +
            LFill(CEP_CP) +
            LFill(CP, 0) +
            LFill(FONE) +
            LFill(FAX) +
            LFill(EMAIL) ) ;
       ///
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0990;
  var strLinha : AnsiString;
begin
  //--Before
  strLinha := '';
  if Assigned(FOnBeforeWriteRegistro0990) then
  begin
    FOnBeforeWriteRegistro0990(strLinha);
    if strLinha <> EmptyStr then
       Add(strLinha);
  end;

  if Assigned(Registro0990) then
  begin
     with Registro0990 do
     begin
       QTD_LIN_0 := QTD_LIN_0 + 1;
       ///
       strLinha := LFill('0990') +
                   LFill(QTD_LIN_0,0);

       if Assigned(FOnWriteRegistro0990) then FOnWriteRegistro0990(strLinha);

       Add(strLinha);
     end;
  end;

  //-- After
  strLinha := '';
  if Assigned(FOnAfterWriteRegistro0990) then
  begin
    FOnAfterWriteRegistro0990(strLinha);
    if strLinha <> EmptyStr then
       Add(strLinha);
  end;
end;

end.
