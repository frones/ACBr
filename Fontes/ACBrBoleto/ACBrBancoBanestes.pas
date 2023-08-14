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

unit ACBrBancoBanestes;

interface

uses
  Classes, SysUtils,
  ACBrBoleto, ACBrBoletoConversao;

type
  { TACBrBancoBanestes }
  TACBrBancoBanestes = class(TACBrBancoClass)
  private
    fSequencialRegistro: Integer;
    fQuantidadeDesconto: Integer;
    fQuantidadeSimples: Integer;
    fQuantidadeCaucionada: Integer;
    fTotalCobrancaDesconto: Double;
    fTotalCobrancaSimples: Double;
    fTotalCobrancaCaucionada: Double;
    fASBACE: string;
    function GetASBACE: string;
  protected
    procedure EhObrigatorioContaDV; override;
    procedure EhObrigatorioAgenciaDV; override;
  public
    Constructor create(AOwner: TACBrBanco);

    property ASBACE: string read GetASBACE write fASBACE;
    function CalcularCampoASBACE(const ACBrTitulo: TACBrTitulo):string;
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;
    Procedure LerRetorno400(ARetorno:TStringList); override;
    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa:TStringList): String; override;
    procedure LerRetorno240(ARetorno:TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia ): String; override;

  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, MaskUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrBancoBanestes }

constructor TACBrBancoBanestes.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 3;
   fpNome                  := 'Banestes';
   fpNumero                := 21;
   fpTamanhoMaximoNossoNum := 8;
   fpTamanhoAgencia        := 3;
   fpTamanhoConta          := 11;
   fpTamanhoCarteira       := 2;
   fpLayoutVersaoArquivo   := 40;
   fpLayoutVersaoLote      := 40;
   fpCodigosMoraAceitos    := '01239';
end;

procedure TACBrBancoBanestes.EhObrigatorioAgenciaDV;
begin
  // sem validação
end;

procedure TACBrBancoBanestes.EhObrigatorioContaDV;
begin
  // sem validação
end;

function TACBrBancoBanestes.CalcularCampoASBACE(
  const ACBrTitulo: TACBrTitulo): string;
var
  cIndice, cLivreAsbace: String;
  nContAsbace: Word;
  nResult, nResultTemp, nDigAsbace01 : Integer;
begin
  { Banestes não usa digitos verificadores para agência e conta }
  cLivreAsbace := copy(ACBrTitulo.NossoNumero,1,8)                       +
                  copy(trim(ACBrTitulo.ACBrBoleto.Cedente.Conta), 1, 11) +
                  IfThen(ACBrtitulo.ACBrBoleto.Cedente.Modalidade = '',
                         '4', ACBrtitulo.ACBrBoleto.Cedente.Modalidade)  +
                  IntToStrZero(fpNumero,3);
  cIndice      := '21212121212121212121212';
  nResult      := 0;
  for nContAsbace := 23 downto 1 do
  begin
     nResultTemp := (StrToInt(cIndice[nContAsbace]) * StrToInt(cLivreAsbace[nContAsbace]));
     if nResultTemp > 9 then
        nResult := nResult + (nResultTemp - 9)
     else
        nResult := nResult + nResultTemp;
  end;

  nResult := nResult Mod 10;
  if nResult > 0 then
     nResult := 10 - nResult
  else nResult:=0;
     nDigAsbace01 := nResult; //guardo o primeiro dig da asbace

  cLivreAsbace := cLivreAsbace + IntToStr(nResult);
  cIndice      := '765432765432765432765432';
  nResult      := 0;

  for nContAsbace := 24 downto 1 do
    nResult := nResult + (StrToInt(cIndice[nContAsbace]) * StrToInt(cLivreAsbace[nContAsbace]));

  nResult := nResult Mod 11;
  if nResult = 0 then
     nResult := 0
  else if nResult = 1 then
   begin
     while nResult = 1 do
     begin
        nDigAsbace01 := nDigAsbace01 + 1;

        if nDigAsbace01 = 10 then
           nDigAsbace01 := 0;
        cLivreAsbace := copy(cLivreAsbace,1,23) + IntToStr(nDigAsbace01);
        cIndice      := '765432765432765432765432';
        nResult      := 0;

        for nContAsbace := 24 downto 1 do
         nResult := nResult + (StrToInt(cIndice[nContAsbace]) * StrToInt(cLivreAsbace[nContAsbace]));

        nResult := nResult Mod 11;
        if nResult = 0 then
          nResult := 0
        else if nResult > 1 then
          nResult := 11 - nResult;
     end;
   end
  else
   if nResult > 1 then
      nResult := 11 - nResult;

  cLivreAsbace := cLivreAsbace + IntToStr(nResult);
  result := cLivreAsbace;
end;

function TACBrBancoBanestes.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
var
   ADigitoNossoNumero : string;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal := 13;
   Modulo.Documento := ACBrTitulo.NossoNumero;
   Modulo.Calcular;
   AdigitoNossoNumero:=IntToStr(Modulo.DigitoFinal);
   Modulo.Documento := ACBrTitulo.NossoNumero+AdigitoNossoNumero;
   Modulo.Calcular;
   Result:= AdigitoNossoNumero+inttostr(Modulo.DigitoFinal);
end;

function TACBrBancoBanestes.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras,FatorVencimento, DigitoCodBarras:String;
begin
  with ACBrTitulo.ACBrBoleto do
  begin
    fASBACE := CalcularCampoASBACE(ACBrTitulo);

    FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);


    CodigoBarras := IntToStrZero(Numero,3) + '9';
    CodigoBarras := CodigoBarras + FatorVencimento;
    CodigoBarras := CodigoBarras + IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10);
    CodigoBarras := CodigoBarras + fASBACE;
    DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
  end;

  Result:= IntToStrZero(Numero,3) + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoBanestes.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.NossoNumero+'-'+CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoBanestes.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   // Banestes não usa digitos verificadores em agência e conta
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+ '/' +
             ACBrTitulo.ACBrBoleto.Cedente.Conta;
end;

function TACBrBancoBanestes.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    // Header Arquivo
    Result :=
      '021' +                                                // Codigo do Banco
      '0000' +                                               // Lote de Servico
      '0' +                                                  // Codigo do Registro Header
      Space(9) +                                             // Filler
      IfThen(TipoInscricao = pJuridica, '2', '1') +          // Tipo de Inscricao da Empresa
      PadLeft(OnlyNumber(CNPJCPF), 14, '0') +                // Numero de Incscricao da Empresa
      Space(20) +                                            // Codigo do Convenio do Banco
      '00000' +                                              // Agencia mantenedora da Conta
      ' ' +                                                  // Digito verificador da agencia
      PadLeft(OnlyNumber(Conta),12,'0') +                    // Numero da Conta Corrente
      '0' +                                                  // Digito verificador da conta
      '0' +                                                  // digito verificador agencia conta
      PadRight(Nome, 30) +                                   // Nome por extenso da empresa
      PadRight('BANESTES',30) +                              // Nome do banco
      Space(10) +                                            // Filler
      '1' +                                                  // Identificacao do arquivo remessa
      FormatDateTime('ddmmyyyy',Now) +                       // data da gravacao do arquivo
      FormatDateTime('hhmmss',Now) +                         // hora da geracao do arquivo
      PadLeft(OnlyNumber(IntToStr(NumeroRemessa)), 6, '0') + // Numero sequencial do arquivo
      PadLeft(IntToStr(fpLayoutVersaoArquivo) , 3, '0')  +   // numero da versao do leiaute do arquivo
      '00000' +                                              // desidade de gravacao do arquivo
      PadRight('REMESSA',7) +                                // Identificacao por extenso do arquivo remessa
      Space(6) +                                             // Codigo identificador de impressao
      Space(10) +                                            // Para uso da carteira de descontos
      Space(20) +                                            // Para uso reservado da empresa
      Space(26);                                             // Filler

      // Header Lote
    Result := Result + sLineBreak +
      '021' +                                                // Codigo do Banco
      '0001' +                                               // Lote de Servico
      '1' +                                                  // Tipo de Registro
      'R' +                                                  // Tipo de Operacao
      '01' +                                                 // Tipo de Servico
      '  ' +                                                 // Filler
      PadLeft(IntToStr(fpLayoutVersaoLote), 3, '0') +        // Numero da versao do leiaute do lote
      ' ' +                                                  // Filler
      IfThen(TipoInscricao = pJuridica, '2', '1') +          // Tipo de inscricao da empresa
      PadLeft(OnlyNumber(CNPJCPF), 15, '0') +                // Numero de inscricao da empresa
      Space(20) +                                            // Codigo do convenio com o banco
      '00000' +                                              // Agencia mantenedora da conta
      ' ' +                                                  // Digito verificador da agencia
      PadLeft(OnlyNumber(Conta),12,'0') +                    // Numero da conta corrente
      '0' +                                                  // Digito verificador da conta
      ' ' +                                                  // Digito verificador da agencia conta
      PadRight(Nome, 30) +                                   // Nome por extenso da empresa
      Space(40) +                                            // Mensagem 1
      Space(40) +                                            // Mensagem 2
      PadLeft(OnlyNumber(IntToStr(NumeroRemessa)), 8, '0') + // Numero da remessa retorno
      FormatDateTime('ddmmyyyy',Now) +                       // Data de gravacao
      PadLeft('', 8, '0') +                                  // Data do Credito
      Space(33);                                             // Filler
  end;

  fSequencialRegistro := 0;
  fQuantidadeSimples := 0;
  fQuantidadeCaucionada := 0;
  fQuantidadeDesconto := 0;
  fTotalCobrancaSimples := 0;
  fTotalCobrancaCaucionada := 0;
  fTotalCobrancaDesconto := 0;
end;

procedure TACBrBancoBanestes.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList );
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do begin
      wLinha:= '0'                                                + // ID do Registro
               '1'                                                + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                                          + // Literal de Remessa
               '01'                                               + // Código do Tipo de Serviço
               PadRight('COBRANCA', 15 )                          +
               PadLeft(OnlyNumber(Copy(Trim(Conta),2,11)+trim(ContaDigito)), 11,'0')+ // Codigo da Empresa no Banco
               space(9)                                           + // COMPLEMENTO DO REGISTRO
               PadRight(Nome, 30)                                 + // Nome da Empresa
               IntToStrzero(Numero,3)                             +
               PadRight('BANESTES', 8)                            + // Código e Nome do Banco(237 - Bradesco)
               space(7)                                           + // COMPLEMENTO DO REGISTRO
               FormatDateTime('ddmmyy',Now)                       +
               Space(294)                                         + // Data de geração do arquivo + brancos
               IntToStrZero(1,6);                                   // Nr. Sequencial de Remessa + brancos + Contador

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;

end;

function TACBrBancoBanestes.GerarRegistroTrailler240(ARemessa: TStringList): String;
begin
  // Trailer do Lote
  Result :=
    '021' +                                                   // Codigo do banco
    '0001' +                                                  // Lote de servico
    '5' +                                                     // Tipo de registro
    Space(9) +                                                // Filler
    IntToStrZero(1 + fSequencialRegistro + 1, 6) +            // Quantidade de registros no lote
    IntToStrZero(fQuantidadeSimples, 6) +                     // Quantidade de titulos Cobranca Simples
    IntToStrZero(Round(fTotalCobrancaSimples * 100), 17) +    // Valor total de titulos Cobranca Simples
    IntToStrZero(0, 3) +                                      // Numero aviso de lancamento Cobranca Simples
    IntToStrZero(0, 6) +                                      // Quantidade de titulos Cobranca Vinculada
    IntToStrZero(0, 17) +                                     // Valor total de titulos Cobranca Vinculada
    IntToStrZero(0, 3) +                                      // Numero aviso de lancamento Cobranca Vinculada
    IntToStrZero(fQuantidadeCaucionada, 6) +                  // Quantidade de titulos Cobranca Caucionada
    IntToStrZero(Round(fTotalCobrancaCaucionada * 100), 17) + // Valor total de titulos Cobranca Caucionada
    IntToStrZero(0, 3) +                                      // Numero aviso de lancamento Cobranca Caucionada
    IntToStrZero(fQuantidadeDesconto, 6) +                    // Quantidade de titulos Cobranca Desconto
    IntToStrZero(Round(fTotalCobrancaDesconto * 100), 17) +   // Valor total de titulos Cobranca Desconto
    IntToStrZero(0, 3) +                                      // Numero aviso de lancamento Cobranca Desconto
    Space(113);                                               // Filler

  // Trailer do Arquivo
  Result := Result + sLineBreak +
    '021' +              // Codigo do banco
    '9999' +             // Lote de servico
    '9' +                // Tipo de registro
    Space(9) +           // Filler
    IntToStrZero(0, 6) + // Quantidade de lotes do arquivo
    IntToStrZero(0, 6) + // Quantidade de registros do arquivo
    IntToStrZero(0, 6) + // Quantidade de contas para conciliacao
    Space(205);          // Filler

end;

procedure TACBrBancoBanestes.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha:= '9' + Space(393)                     + // ID Registro
            IntToStrZero( ARemessa.Count + 1, 6);  // Contador de Registros

   aRemessa.Text := aRemessa.Text + UpperCase(wLinha);

end;

function TACBrBancoBanestes.GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String;
  function ResponEmissaoToCod(ResponEmissao: TACBrResponEmissao): string;
  begin
    case ResponEmissao of
      tbBancoEmite: Result := '1';
      tbCliEmite: Result := '2';
    else
      Result := '4';
    end;
  end;
  function CarteiraEnvioToCod(CarteiraEnvio: TACBrCarteiraEnvio): string;
  begin
    if CarteiraEnvio = tceCedente then
      Result := '2'
    else
      Result := '1';
  end;
  function EspecieDocToCod(EspecieDoc: string): string;
  var
    cod: Integer;
  begin
    cod := AnsiIndexStr(EspecieDoc, ['CH','DM','DMI','DS','DSI','DR','LC','NCC','NCE',
       'NCI','NCR','NP','NPR','TM','TS','NS','RC','FAT', 'ND','AP','ME','PC','NF','DD']) + 1;

    if cod = 0 then
      cod := 99;

    Result := IntToStrZero(cod, 2);
  end;
  function CodigoNegativacaoToCod(CodigoNegativacao: TACBrCodigoNegativacao): string;
  begin
    case CodigoNegativacao of
      cnProtestarCorrido: Result := '1';
      cnProtestarUteis: Result := '2';
      cnNaoProtestar: Result := '3';
      cnCancelamento: Result := '9';
    else
      Result := '0';
    end;
  end;
  function SacadorAvalistaPessoaToCod(Pessoa: TACBrPessoa): string;
  begin
    case Pessoa of
      pFisica: Result := '1';
      pJuridica: Result := '2';
    else
      Result := '0';
    end;
  end;
  function LinhaMensagem(Index: Integer): string;
  begin
    if ACBrTitulo.Mensagem.Count > Index then
      Result := PadRight(ACBrTitulo.Mensagem[Index], 40)
    else
      Result := Space(40);
  end;
  function CorrigirCarteira(Carteira: string): string;
  begin
    case StrToIntDef(Carteira, -1) of
      01: Result := '1'; // 01 - Desconto
      11: Result := '1'; // 11 - Simples
      13: Result := '3'; // 13 - Caucionada
    else
      Result := Carteira;
    end;
  end;
begin
  // Calcular totais para trailler
  if ACBrTitulo.Carteira = '01' then
  begin
    Inc(fQuantidadeDesconto);
    fTotalCobrancaDesconto := fTotalCobrancaDesconto + ACBrTitulo.ValorDocumento;
  end else
  if (ACBrTitulo.Carteira = '13') or (StrToIntDef(ACBrTitulo.Carteira, -1) = 3) then
  begin
    Inc(fQuantidadeCaucionada);
    fTotalCobrancaCaucionada := fTotalCobrancaCaucionada + ACBrTitulo.ValorDocumento;
  end else
  begin
    Inc(fQuantidadeSimples);
    fTotalCobrancaSimples := fTotalCobrancaSimples + ACBrTitulo.ValorDocumento;
  end;

  // Segmento P
  Inc(fSequencialRegistro);
  Result :=
    '021' +                                                                                           // Codigo do banco
    '0001' +                                                                                          // Lote do Servivo
    '3' +                                                                                             // Tipo de registro
    IntToStrZero(fSequencialRegistro, 5) +                                                            // Sequencia do regsitro no lote
    'P' +                                                                                             // Codigo do segmento do registro detalhe
    ' ' +                                                                                             // Filler
    TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo) +                                  // Identificacao da Ocorrencia remessa
    '00000' +                                                                                         // Agencia mantenedora da conta
    ' ' +                                                                                             // Digito verificador da agencia
    PadLeft(OnlyNumber(ACBrBanco.ACBrBoleto.Cedente.Conta), 12, '0') +                                // Numero da conta corrente
    '0' +                                                                                             // Digito verificador da conta
    ' ' +                                                                                             // Digito verificador da agencia conta
    PadLeft(ACBrTitulo.NossoNumero + CalcularDigitoVerificador(ACBrTitulo), 10, '0') +                // Nosso Numero
    Space(10) +                                                                                       // Filler
    PadRight(CorrigirCarteira(ACBrTitulo.Carteira), 1) +                                              // Codigo da Carteira
    '1' +                                                                                             // Forma de cadastro do titulo no banco
    ' ' +                                                                                             // Tipo de Documento
    ResponEmissaoToCod(ACBrBanco.ACBrBoleto.Cedente.ResponEmissao) +                                  // Identificacao da emissao do boleto
    CarteiraEnvioToCod(ACBrTitulo.CarteiraEnvio) +                                                    // Identificacao da distribuicao
    PadRight(ACBrTitulo.NumeroDocumento, 15, ' ') +                                                   // Numero do documento de cobranca
    FormatDateTime('ddmmyyyy', ACBrTitulo.Vencimento) +                                               // Data de vencimento do titulo
    IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 15) +                                        // Valor nominal do titulo
    '00000' +                                                                                         // Agencia encarregada da cobranca
    '0' +                                                                                             // Digito verificador da agencia
    EspecieDocToCod(ACBrTitulo.EspecieDoc) +                                                          // Especie do titulo
    IfThen(ACBrTitulo.Aceite = atSim, 'A', 'N') +                                                     // Identificacao de aceito ou nao aceito
    FormatDateTime('ddmmyyyy', ACBrTitulo.DataDocumento) +                                            // Data de emissao do titulo
    PadRight(ACBrTitulo.CodigoMora, 1) +                                                              // Codigo do juros de mora
    IfThen(ACBrTitulo.CodigoMora = '3', PadLeft('', 8, '0'),
      FormatDateTime('ddmmyyyy', ACBrTitulo.DataMoraJuros)) +                                         // Data do juros de mora
    IntToStrZero(Round(ACBrTitulo.ValorMoraJuros * 100), 15) +                                        // Juros de mora po dia taxa
    TipoDescontoToString(ACBrTitulo.TipoDesconto) +                                                   // Codigo do desconto 1
    IfThen(ACBrTitulo.TipoDesconto in [tdNaoConcederDesconto, tdCancelamentoDesconto],
      PadLeft('', 8, '0'), FormatDateTime('ddmmyyyy', ACBrTitulo.DataDesconto)) +                     // Data limite para desconto 1
    IntToStrZero(Round(ACBrTitulo.ValorDesconto * 100), 15) +                                         // Valor percentual desconto
    IntToStrZero(Round(ACBrTitulo.ValorIOF * 100), 15) +                                              // Valor do IOF
    IntToStrZero(Round(ACBrTitulo.ValorAbatimento * 100), 15) +                                       // Valor do abatimento
    PadRight(ACBrTitulo.SeuNumero, 25) +                                                              // Identificacao do titulo na empresa
    CodigoNegativacaoToCod(ACBrTitulo.CodigoNegativacao) +                                            // Codigo para protesto
    IntToStrZero(ACBrTitulo.DiasDeProtesto, 2) +                                                      // Numero dias para protesto
    IfThen((ACBrTitulo.DataBaixa > 0) and (ACBrTitulo.DataBaixa > ACBrTitulo.Vencimento), '1', '2') + // Codigo para baixa devolucao
    IfThen((ACBrTitulo.DataBaixa > 0) and (ACBrTitulo.DataBaixa > ACBrTitulo.Vencimento),
      IntToStrZero(DaysBetween(ACBrTitulo.DataBaixa, ACBrTitulo.Vencimento), 3), '000') +             // Numero de dias para baixa devolucao
    '09' +                                                                                            // Codigo da moeda
    IntToStrZero(0, 10) +                                                                             // Numero do contrato da operacao de credito
    ' ';                                                                                              // Filler

  // Segmento Q
  Inc(fSequencialRegistro);
  Result := Result + sLineBreak +
    '021' +                                                                  // Codigo do banco
    '0001' +                                                                 // Lote de Servico
    '3' +                                                                    // Tipo de registro
    IntToStrZero(fSequencialRegistro, 5) +                                   // Sequencia do regsitro no lote
    'Q' +                                                                    // Codigo do segmento do registro detalhe
    ' ' +                                                                    // Filler
    TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo) +         // Identificacao da Ocorrencia remessa
    IfThen(ACBrTitulo.Sacado.Pessoa = pJuridica, '2', '1') +                 // Tipo de inscricao do sacado
    PadLeft(OnlyNumber(ACBrTitulo.Sacado.CNPJCPF), 15, '0') +                // Numero de inscricao do sacado
    PadRight(ACBrTitulo.Sacado.NomeSacado, 40) +                             // Nome do sacado
    PadRight(ACBrTitulo.Sacado.Logradouro, 40) +                             // Endereco do sacado
    PadRight(ACBrTitulo.Sacado.Bairro, 15) +                                 // Bairro do sacado
    PadLeft(ACBrTitulo.Sacado.CEP, 8, '0') +                                 // CEP do sacado
    PadRight(ACBrTitulo.Sacado.Cidade, 15) +                                 // Cidade do sacado
    PadRight(ACBrTitulo.Sacado.UF, 2) +                                      // Unidade de federacao do sacado
    SacadorAvalistaPessoaToCod(ACBrTitulo.Sacado.SacadoAvalista.Pessoa) +    // Tipo de inscricao sacador avalista
    PadLeft(OnlyNumber(ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF), 15, '0') + // Numero de inscricao sacador avalista
    PadRight(ACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 40) +            // Nome sacador avalista
    '000' +                                                                  // Codigo banco correspondente na compensacao
    Space(6) +                                                               // Identificacao do carne
    '00' +                                                                   // Numero de parcelas do carne
    '00' +                                                                   // Quantide de parcelas do carne
    Space(10) +                                                              // Nosso numero banco correspondente
    Space(8);                                                                // Filler

  // Segmento R
  Inc(fSequencialRegistro);
  Result := Result + sLineBreak +
    '021' +                                                                             // Codigo do banco
    '0001' +                                                                            // Lote de servico
    '3' +                                                                               // Tipo de registro
    IntToStrZero(fSequencialRegistro, 5) +                                              // Sequencia do regsitro no lote
    'R' +                                                                               // Codigo do segmento do registro detalhe
    ' ' +                                                                               // Uso exclusivo FEBRABAN
    TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo) +                    // Identificacao da Ocorrencia remessa
    TipoDescontoToString(ACBrTitulo.TipoDesconto2) +                                    // Codigo do desconto 2
    IfThen(ACBrTitulo.TipoDesconto2 in [tdNaoConcederDesconto, tdCancelamentoDesconto],
      PadLeft('', 8, '0'), FormatDateTime('ddmmyyyy', ACBrTitulo.DataDesconto2)) +      // Data do desconto 2
    IntToStrZero(Round(ACBrTitulo.ValorDesconto2 * 100), 15) +                          // Valor do desconto 2
    '0' +                                                                               // Codigo do desconto 3
    IntToStrZero(0, 8) +                                                                // Data do desconto 3
    IntToStrZero(0, 15) +                                                               // Valor do desconto 3
    IfThen(ACBrTitulo.CodigoMulta = cmValorFixo, '1', '2') +                            // Codigo da multa
    IfThen(ACBrTitulo.DataMulta > 0, FormatDateTime('ddmmyyyy',
      ACBrTitulo.DataMulta), IntToStrZero(0, 8)) +                                      // Data da multa
    IntToStrZero(Round(ACBrTitulo.PercentualMulta * 100), 15) +                         // Valor da multa
    Space(10) +                                                                         // Informacao ao sacado
    LinhaMensagem(0) +                                                                  // Mensagem 3
    LinhaMensagem(1) +                                                                  // Mensagem 4
    Space(20) +                                                                         // Uso exclusivo FEBRABAN
    IntToStrZero(0, 8) +                                                                // Codigo de ocorrencia do sacado
    IntToStrZero(0, 3) +                                                                // Codigo do banco na conta do debito
    IntToStrZero(0, 5) +                                                                // Codigo da agencia do debito
    ' ' +                                                                               // Digito verificador da agencia
    IntToStrZero(0, 12) +                                                               // Conta corrente para  debito
    '0' +                                                                               // Digito verificador da conta
    '0' +                                                                               // Digito verificador da agencia conta
    '0' +                                                                               // Aviso para debito automatico
    Space(9);                                                                           // Uso exclusivo FEBRABAN

  // Segmento S
  Inc(fSequencialRegistro);
  Result := Result + sLineBreak +
    '021' +                                                          // Codigo do banco
    '0001' +                                                         // Lote de servico
    '3' +                                                            // Tipo de registro
    IntToStrZero(fSequencialRegistro, 5) +                           // Sequencia do regsitro no lote
    'S' +                                                            // Codigo do segmento do registro detalhe
    ' ' +                                                            // Filler
    TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo) + // Identificacao da Ocorrencia remessa
    '3' +                                                            // Identificacao da impressao
    LinhaMensagem(0) +                                               // Mensagem 5
    LinhaMensagem(1) +                                               // Mensagem 6
    LinhaMensagem(2) +                                               // Mensagem 7
    LinhaMensagem(3) +                                               // Mensagem 8
    LinhaMensagem(4) +                                               // Mensagem 9
    Space(22);                                                       // Filler
end;

procedure TACBrBancoBanestes.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
   ATipoInscricao, TipoBoleto, ATipoAceite: String;
   DigitoNossoNumero, Ocorrencia: String;
   PracaPostagem, aCarteira, Protesto: String;
   TipoSacado, MensagemCedente, wLinha: String;
   TipoAvalista: String;
   TipoMora: String;
begin

   case ACBrBanco.ACBrBoleto.Cedente.TipoInscricao of
      pFisica  : ATipoInscricao := '01';
      pJuridica: ATipoInscricao := '02';
   end;

   with ACBrTitulo do
   begin
      DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      aCarteira := IntToStrZero(StrToIntDef(Trim(Carteira), 0), 1);

      {Pegando Código da Ocorrencia}
      Ocorrencia := TipoOcorrenciaToCodRemessa(OcorrenciaOriginal.Tipo);

      {Pegando Tipo de Boleto}
      case ACBrBoleto.Cedente.ResponEmissao of
         tbCliEmite        : TipoBoleto  := '01';
         tbBancoNaoReemite : TipoBoleto  := '01';
         tbBancoEmite      : TipoBoleto  := '21';
         tbBancoReemite    : TipoBoleto  := '21';
      end;

      case ACBrBoleto.Cedente.ResponEmissao of
         tbCliEmite        : PracaPostagem  := '00501';
         tbBancoNaoReemite : PracaPostagem  := '00501';
         tbBancoEmite      : PracaPostagem  := '00000';
         tbBancoReemite    : PracaPostagem  := '00000';
      end;

      {Pegando campo Intruções}
      if (DataProtesto > 0) and (DataProtesto > Vencimento) then
          Protesto := 'P6' + IntToStrZero(DaysBetween(DataProtesto, Vencimento), 2)
      else
         Protesto := PadLeft(Trim(Instrucao1), 2, '0') + PadLeft(Trim(Instrucao2), 2, '0');

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : TipoSacado := '01';
         pJuridica : TipoSacado := '02';
      else
         TipoSacado := '99';
      end;

      {Pegando Tipo de Avalista}
      if Sacado.SacadoAvalista.CNPJCPF <> '' then
       begin
        case Sacado.SacadoAvalista.Pessoa of
           pFisica   : TipoAvalista := '1';
           pJuridica : TipoAvalista := '2';
        else
           TipoAvalista := '9';
        end;
       end
      else
        TipoAvalista:= '0';

      case Aceite of
         atSim :  ATipoAceite := 'A';
      else
         ATipoAceite := 'N';
      end;

      case CodigoMoraJuros of
        cjValorDia   : TipoMora := '0';
      else
        TipoMora := '1';
      end;

      with ACBrBoleto do
      begin
        if Mensagem.Text<> ''then
          MensagemCedente:= Mensagem[0];
        wLinha := '1'                                                         +  // ID Registro
                  ATipoInscricao                                              +  // TIPO INSCRICAO EMPRESA(CNPJ, CPF);
                  PadRight(OnlyNumber(Cedente.CNPJCPF), 14, '0')              +
                  PadLeft(OnlyNumber(Copy(Trim(Cedente.Conta),2,11)+trim(cedente.ContaDigito)), 11, '0')+ // Codigo da Empresa no Banco
                  Space(9)                                                    +
                  PadRight(SeuNumero,25)                                       +  // identificacao da operacao na empresa
                  PadRight(Copy(NossoNumero, 1, 8) +
                           DigitoNossoNumero, 10, '0')                        +
                  IfThen(PercentualMulta > 0, '1', '0')                       +  // Indica se exite Multa ou não
                  IntToStrZero( round( PercentualMulta * 100 ), 9)            +  // Percentual de Multa formatado com 2 casas decimais
                  Space(06)                                                   +  // identificação do carnê
                  '00'                                                        +  // número da parcela do carnê
                  '00'                                                        +  // quantidade de parcelas no carnê
                  TipoAvalista                                                +  // tipo do sacador avalista
                  PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF),14,'0')   +  // sacador avalista. não pode ser o proprio sacado
                  aCarteira                                                   +
                  Ocorrencia                                                  +
                  PadRight(NumeroDocumento, 10)                               +
                  FormatDateTime('ddmmyy', Vencimento)                        +
                  '000'                                                       +
                  IntToStrZero(Round(ValorDocumento * 100 ), 10)              +
                  IntToStrzero(Numero, 3)                                     +  // código do banco
                  PracaPostagem                                               +
                  TipoBoleto                                                  +
                  ATipoAceite                                                 +
                  FormatDateTime('ddmmyy', DataDocumento )                    +  // Data de Emissão
                  Protesto                                                    +
                  TipoMora                                                    +  // Indica se exite Multa ou não
                  IntToStrZero(Round(ValorMoraJuros * 100 ), 12)              +
                  IfThen(DataDesconto < EncodeDate(2000, 01, 01), '000000',
                         FormatDateTime('ddmmyy', DataDesconto))              +
                  IntToStrZero(Round( ValorDesconto * 100 ), 13)              +
                  IntToStrZero(Round( ValorIOF * 100 ), 13)                   +
                  IntToStrZero(Round( ValorAbatimento * 100 ), 13)            +
                  TipoSacado                                                  +
                  PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')                  +
                  PadRight(Sacado.NomeSacado, 40, ' ')                        +
                  PadRight(Sacado.Logradouro + Sacado.Numero, 40)             +
                  PadRight(Sacado.Bairro, 12)                                 +
                  PadRight(Sacado.CEP, 8)                                     +
                  PadRight(Sacado.Cidade,15)                                  +
                  PadRight(Sacado.UF, 2)                                      +
                  PadRight(MensagemCedente, 40)                               +
                  '00'                                                        +
                  '0';

        wLinha := wLinha + IntToStrZero(aRemessa.Count + 1 {ListadeBoletos.IndexOf(ACBrTitulo) +
                               ListadeBoletos.IndexOf(ACBrTitulo) + 2}, 6);
        aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
      end;
   end;

end;

function TACBrBancoBanestes.GetASBACE: string;
begin
  Result := copy(fASBACE,1,4)+' '+ copy(fASBACE,5,4)+' '+
                          copy(fASBACE,9,4)+' '+ copy(fASBACE,13,4)+' '+
                          copy(fASBACE,17,4)+' '+ copy (fASBACE,21,4)+' '+
                          copy(fASBACE,25,1);
end;

procedure TACBrBancoBanestes.LerRetorno240(ARetorno: TStringList);
  function CodToACBrPessoa(Codigo: Char): TACBrPessoa;
  begin
    case Codigo of
      '1': Result := pFisica;
      '2': Result := pJuridica;
    else
      Result := pOutras
    end;
  end;
  function CodToEspecieTitulo(Codigo: string): string;
  begin
    case StrToIntDef(Codigo, -1) of
      01: Result := 'CH';
      02: Result := 'DM';
      03: Result := 'DMI';
      04: Result := 'DS';
      05: Result := 'DSI';
      06: Result := 'DR';
      07: Result := 'LC';
      08: Result := 'NCC';
      09: Result := 'NCE';
      10: Result := 'NCI';
      11: Result := 'NCR';
      12: Result := 'NP';
      13: Result := 'NPR';
      14: Result := 'TM';
      15: Result := 'TS';
      16: Result := 'NS';
      17: Result := 'RC';
      18: Result := 'FAT';
      19: Result := 'ND';
      20: Result := 'AP';
      21: Result := 'ME';
      22: Result := 'PC';
      23: Result := 'NF';
      24: Result := 'DD';
      32: Result := 'BDP';
    else
      Result := '';
    end;
  end;
var
  I, idxMotivoOcor: Integer;
  Titulo: TACBrTitulo;
begin
  if ARetorno.Count = 0 then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno + ' está vazio.'));

  Titulo := nil;
  for I := 0 to ARetorno.Count - 1 do
  begin
    // Header do arquivo
    if (Length(ARetorno[I]) = 240) and (ARetorno[I][8] = '0') then
    begin
      if (StrToIntDef(Copy(ARetorno[I],1,3),-1) <> 21) then
        raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
          ' não é um arquivo do ' + Nome));

      if not ACBrBanco.ACBrBoleto.LeCedenteRetorno then
      begin
        if StrToFloatDef(Copy(ARetorno[I], 19, 14), 0) <>
          StrToFloatDef(OnlyNumber(ACBrBanco.ACBrBoleto.Cedente.CNPJCPF), -1) then
           raise Exception.Create(ACBrStr('CNPJ\CPF do arquivo inválido.'));

        if StrToFloatDef(Copy(ARetorno[I], 59, 12), 0) <>
          StrToFloatDef(OnlyNumber(ACBrBanco.ACBrBoleto.Cedente.Conta), -1) then
           raise Exception.Create(ACBrStr('Conta do arquivo inválida.'));
      end else
      begin
        ACBrBanco.ACBrBoleto.Cedente.Conta := FloatToStr(StrToFloatDef(Copy(ARetorno[I], 59, 12),0));
        ACBrBanco.ACBrBoleto.Cedente.Nome := Copy(ARetorno[I], 73, 30);

        if ARetorno[I][18] = '1' then
        begin
          ACBrBanco.ACBrBoleto.Cedente.TipoInscricao := pFisica;
          ACBrBanco.ACBrBoleto.Cedente.CNPJCPF := Copy(ARetorno[I], 22, 11);
        end
        else
        begin
          ACBrBanco.ACBrBoleto.Cedente.TipoInscricao := pJuridica;
          ACBrBanco.ACBrBoleto.Cedente.CNPJCPF := Copy(ARetorno[I], 19, 14);
        end;
      end;

      ACBrBanco.ACBrBoleto.DataArquivo :=
        StrToDateDef(FormatMaskText('00/00/0000;0', Copy(ARetorno[I], 144, 8)), Date);

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
    end;

    // Header do lote
    if (Length(ARetorno[I]) = 240) and (ARetorno[I][8] = '1') then
    begin
      if (ARetorno[I][9] <> 'T') then
        raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
          ' não é um arquivo de retorno.'));
    end;

    // Detalhe "T"
    if (Length(ARetorno[I]) = 240) and (ARetorno[I][8] = '3') and (ARetorno[I][14] = 'T') then
      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    if Assigned(Titulo) then
    begin
      if (Length(ARetorno[I]) = 240) and (ARetorno[I][8] = '3') and (ARetorno[I][14] = 'T') then
      begin

        Titulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(Copy(ARetorno[I],16,2),0));
        Titulo.NossoNumero := Copy(ARetorno[I], 38, 8);
        Titulo.Carteira := ARetorno[I][58];
        Titulo.NumeroDocumento := Copy(ARetorno[I], 59, 15);
        Titulo.Vencimento := StrToDateDef(FormatMaskText('00/00/0000;0', Copy(ARetorno[I], 74, 8)),
        	ACBrBanco.ACBrBoleto.DataArquivo);
        Titulo.ValorDocumento := StrToFloatDef(Copy(ARetorno[I], 82, 15), 0) / 100;
        Titulo.SeuNumero := Copy(ARetorno[I], 106, 25);
        Titulo.ValorDespesaCobranca := StrToFloatDef(Copy(ARetorno[I], 199, 15), 0) / 100;

        Titulo.Sacado.Pessoa := CodToACBrPessoa(ARetorno[I][133]);

        case Titulo.Sacado.Pessoa of
          pFisica: Titulo.Sacado.CNPJCPF := Copy(ARetorno[I], 138, 11);
          pJuridica: Titulo.Sacado.CNPJCPF := Copy(ARetorno[I], 135, 14);
        else
          Titulo.Sacado.CNPJCPF := Trim(Copy(ARetorno[I], 134, 15));
        end;

        Titulo.Sacado.NomeSacado := Copy(ARetorno[I], 149, 40);

        idxMotivoOcor := 214;
        while idxMotivoOcor <= 220 do
        begin
          if Trim(Copy(ARetorno[I], idxMotivoOcor, 2)) <> '' then
          begin
            Titulo.MotivoRejeicaoComando.Add(Copy(ARetorno[I], idxMotivoOcor, 2));
            Titulo.DescricaoMotivoRejeicaoComando.Add(
               ACBrBanco.CodMotivoRejeicaoToDescricao(Titulo.OcorrenciaOriginal.Tipo,
              StrToIntDef(Copy(ARetorno[I], idxMotivoOcor, 2),0)));
          end;

          Inc(idxMotivoOcor, 2);
        end;

        Titulo.EspecieDoc := CodToEspecieTitulo(Copy(ARetorno[I], 222, 2));
      end;

      if (Length(ARetorno[I]) = 240) and (ARetorno[I][8] = '3') and (ARetorno[I][14] = 'U') then
      begin
        Titulo.ValorMoraJuros := StrToFloatDef(Copy(ARetorno[I], 18, 15), 0) / 100;
        Titulo.ValorDesconto := StrToFloatDef(Copy(ARetorno[I], 33, 15), 0) / 100;
        Titulo.ValorAbatimento := StrToFloatDef(Copy(ARetorno[I], 48, 15), 0) / 100;
        Titulo.ValorIOF := StrToFloatDef(Copy(ARetorno[I], 63, 15), 0) / 100;
        Titulo.ValorPago := StrToFloatDef(Copy(ARetorno[I], 78, 15), 0) / 100;
        Titulo.ValorRecebido := StrToFloatDef(Copy(ARetorno[I], 93, 15), 0) / 100;
        Titulo.ValorOutrasDespesas := StrToFloatDef(Copy(ARetorno[I], 108, 15), 0) / 100;
        Titulo.ValorOutrosCreditos := StrToFloatDef(Copy(ARetorno[I], 123, 15), 0) / 100;
        Titulo.DataOcorrencia := StrToDateDef(FormatMaskText('00/00/0000;0', Copy(ARetorno[I], 138, 8)), ACBrBanco.ACBrBoleto.DataArquivo);
      end;

    end;

  end;
end;

procedure TACBrBancoBanestes.LerRetorno400(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;

  Linha,rCedente: String ;
  rCNPJCPF,rConta: String;

  CodOCorrencia: Integer;
  i, MotivoLinha : Integer;
begin

   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],47,30));
   rConta   := trim(Copy(ARetorno[0],27,11));


   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      1 : rCNPJCPF:= Copy(ARetorno[1],07,11);
      2 : rCNPJCPF:= Copy(ARetorno[1],04,14);
   else
      rCNPJCPF:= Copy(ARetorno[1],4,14);
   end;

   with ACBrBanco.ACBrBoleto do
   begin
      if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
         raise Exception.Create(ACBrStr('CNPJ\CPF do arquivo inválido'));

      if (not LeCedenteRetorno) and
          (rConta <> RightStr(OnlyNumber(Cedente.Conta), Length(rConta))) then
         raise Exception.Create(ACBrStr('Agencia\Conta do arquivo inválido'));

      case StrToIntDef(Copy(ARetorno[1],2,2),0) of
         01: Cedente.TipoInscricao:= pFisica;
         else
            Cedente.TipoInscricao:= pJuridica;
      end;

      Cedente.Nome    := rCedente;
      Cedente.CNPJCPF := rCNPJCPF;
      Cedente.Conta   := rConta;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if Copy(Linha,1,1)<> '1' then
         Continue;

      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
         SeuNumero                   := copy(Linha,38,25);
         NumeroDocumento             := copy(Linha,117,10);

         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(copy(Linha,109,2),0));

         MotivoLinha := 319;//posição inicial
         for i := 0 to 4 do
         begin
           //MotivoRejeicaoComando.Add(copy(Linha,MotivoLinha,2));
           MotivoRejeicaoComando.Add(IfThen(copy(Linha,MotivoLinha,2) = '  ',
                                             '00',copy(Linha,MotivoLinha,2)));

           if MotivoRejeicaoComando[i] <> '00' then
           begin
              CodOCorrencia:= StrToIntDef(MotivoRejeicaoComando[i],0) ;
              DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                OcorrenciaOriginal.Tipo,CodOCorrencia));
           end;

           MotivoLinha := MotivoLinha + 2;
         end;

         DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                                Copy(Linha,113,2)+'/'+
                                                Copy(Linha,115,2),0, 'DD/MM/YY' );

         {Espécie do documento}
         if Trim(Copy(Linha,174,2)) = '' then
            EspecieDoc := '99'
         else
            case StrToIntDef(Copy(Linha,174,2),0) of
               01 : EspecieDoc := 'DM';
               02 : EspecieDoc := 'NP';
               03 : EspecieDoc := 'NS';
               04 : EspecieDoc := 'CS';
               05 : EspecieDoc := 'RC';
               10 : EspecieDoc := 'LC';
               11 : EspecieDoc := 'DS';
               21 : EspecieDoc := 'DM';
               22 : EspecieDoc := 'NP';
               23 : EspecieDoc := 'NS';
               24 : EspecieDoc := 'CS';
               25 : EspecieDoc := 'RC';
               30 : EspecieDoc := 'LC';
               31 : EspecieDoc := 'DS';
               39 : EspecieDoc := 'DV';
               99 : EspecieDoc := 'DV';
            else
               EspecieDoc := 'DV';
            end;

         Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                            Copy(Linha,149,2)+'/'+
                                            Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,156,10),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
		 ValorPago            := StrToFloatDef(Copy(Linha,254,13),0)/100;//valor pago pelo cliente																 
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
         ValorRecebido        := (StrToFloatDef(Copy(Linha,254,13),0)/100) - (StrToFloatDef(Copy(Linha,176,13),0)/100);//valor recebido na conta 
         NossoNumero          := Copy(Linha,63,8);
         Carteira             := Copy(Linha,108,1);
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;

         if StrToIntDef(Copy(Linha,111,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                               Copy(Linha,113,2)+'/'+
                                               Copy(Linha,115,2),0, 'DD/MM/YY' );

         if StrToIntDef(Copy(Linha,111,6),0) <> 0 then
            DataBaixa := StringToDateTimeDef(Copy(Linha,111,2)+'/'+
                         Copy(Linha,113,2)+'/'+
                         Copy(Linha,115,2),0,'DD/MM/YY');

         CodigoLiquidacao := Copy(Linha,83,2); //Código Lançamento (Aviso de Movimentação)
      end;
   end;
end;

function TACBrBancoBanestes.TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRemessaRegistrar:                            Result := '01';
      toRemessaBaixar:                               Result := '02';
      toRemessaConcederAbatimento:                   Result := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento:                   Result := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento:                    Result := '06'; {Alteração de vencimento}
      toRemessaConcederDesconto:                     Result := '07';
      toRemessaCancelarDesconto:                     Result := '08'; {Alteração de seu número}
      toRemessaProtestar:                            Result := '09'; {Pedido de protesto}
      toRemessaCancelarInstrucaoProtestoBaixa:       Result := '10'; {Sustar protesto e baixar}
      toRemessaCancelarInstrucaoProtesto:            Result := '11';
      toRemessaAlterarJurosMora:                     Result := '12';
      toRemessaNaoCobrarJurosMora:                   Result := '13';
      toRemessaAlterarMulta:                         Result := '14';
      toRemessaDispensarMulta:                       Result := '15';
      toRemessaAlterarDesconto:                      Result := '16';
      toRemessaAlterarValorAbatimento:               Result := '18';
      toRemessaAlterarNumeroTituloBeneficiario:      Result := '21';
      toRemessaAlterarDadosPagador:                  Result := '23';
      toRemessaAlterarDadosSacadorAvalista:          Result := '24';
      toRemessaRecusaAlegacaoPagador:                Result := '30';
      toRemessaAlterarOutrosDados:                   Result := '31';
      toRemessaAlterarDadosRateioCredito:            Result := '33';
      toRemessaPedidoCancelamentoDadosRateioCredito: Result := '34';
      toRemessaPedidoDesagendamentoDebietoAutom:     Result := '35';
    else
       Result := '01'; {Remessa}
    end;
  end else
  begin
    case TipoOcorrencia of
      toRemessaRegistrar:                      Result := '01';
      toRemessaBaixar:                         Result := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento:             Result := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento:             Result := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento:              Result := '06'; {Alteração de vencimento}
      toRemessaAlterarUsoEmpresa:              Result := '07';
      toRemessaAlterarSeuNumero:               Result := '08'; {Alteração de seu número}
      toRemessaProtestar:                      Result := '09'; {Pedido de protesto}
      toRemessaNaoProtestar:                   Result := '10';
      toRemessaNaoCobrarJurosMora:             Result := '11';
      toRemessaConcederDesconto:               Result := '12';
      toRemessaCancelarDesconto:               Result := '13';
      toRemessaAlterarDesconto:                Result := '14';
      toRemessaAlterarMulta:                   Result := '15';
      toRemessaDispensarMulta:                 Result := '16';
      toRemessaAlterarJurosMora:               Result := '17';
      toRemessaCancelarInstrucaoProtesto:      Result := '18'; {Sustar protesto e manter na carteira}
      toRemessaAlterarValorAbatimento:         Result := '19';
      toRemessaCancelarInstrucaoProtestoBaixa: Result := '20'; {Sustar protesto e baixar}
      toRemessaAlterarDadosPagador:            Result := '23';
      toRemessaAlterarDadosSacadorAvalista:    Result := '24';
      // 34, 75, 76 ??
    else
       Result := '01';                                           {Remessa}
    end;
  end;
end;

function TACBrBancoBanestes.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    04: Result := '04-Transferência de Carteira - Entrada';
    05: Result := '05-Transferencia Carteira - Baixa';
    06: Result := '06-Liquidação';
    07: Result := '07-Confirmação do Recebimento da Instrução de Desconto';
    08: Result := '08-Confirmação do Recebimento do Cancelamento do Desconto';
    09: Result := '09-Baixa';
    11: Result := '11-Título em Carteira(Em Ser)';
    12: Result := '12-Confirmação do Recebimento de Instruções de Abatimento';
    13: Result := '13-Confirmação do Recebimento de Instrução de Cancelamento de Abatimento';
    14: Result := '14-Confirmação do Recebimento de Instrução de Alteração de Vencimento';
    17: Result := '17-Liquidação após Baixa ou Liquidação de Título não Registrado';
    19: Result := '19-Confirmação de Recebimento de Instrução de Protesto';
    20: Result := '20-Confirmação de Recebimento de Instrução de Sustação/Cancelamento de Protesto' ;
    21: Result := '21-Solicitação de Segunda Via de Instrumento de Protesto';
    22: Result := '22-Segunda Via de Instrumento de Protesto Emitida pelo Cartório';
    23: Result := '23-Remessa a Cartório';
    24: Result := '24-Retirada de Cartório e Manutenção em Carteira';
    25: Result := '25-Protestado e Baixado';
    26: Result := '26-Instrução Rejeitada';
    27: Result := '27-Confirmação do Pedido de Alteração Outros Dados';
    28: Result := '28-Débito de Tarifas/Custas';
    29: Result := '29-Ocorrência do Sacado';
    30: Result := '30-Alteração de Outros Dados Rejeitada';
    33: Result := '33-Confirmação da Alteração dos Dados do Rateio de Crédito';
    34: Result := '34-Confirmação do Cancelamento dos Dados do Rateio de Crédito';
    35: Result := '35-Confirmação do Cancelamento do Débito Automático Agendado';
    40: Result := '40-Confirmação da Alteração do Número do Título Dado pelo Cedente';
    42: Result := '42-Confirmação da Alteração dos Dados do Sacado';
    43: Result := '43-Confirmação da Alteração dos Dados do Sacador Avalista';
    51: Result := '51-Título DDA Reconhecido pelo Sacado';
    52: Result := '52-Título DDA Não Reconhecido pelo Sacado';
    53: Result := '53-Título DDA Recusado pela CIP';
    98: Result := '98-Instrução de Protesto Processada';
    99: Result := '99-Remessa Rejeitada';
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoBanestes.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    04: Result := toRetornoTransferenciaCarteiraEntrada;
    05: Result := toRetornoTransferenciaCarteiraBaixa;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
    08: Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
    09: Result := toRetornoBaixado;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoSegundaViaInstrumentoProtesto;
    22: Result := toRetornoSegundaViaInstrumentoProtestoCartorio;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    25: Result := toRetornoBaixaPorProtesto;
    26: Result := toRetornoInstrucaoRejeitada;
    27: Result := toRetornoDadosAlterados;
    28: Result := toRetornoDebitoTarifas;
    29: Result := toRetornoOcorrenciasDoSacado;
    30: Result := toRetornoAlteracaoDadosRejeitados;
    33: Result := toRetornoAcertoDadosRateioCredito;
    34: Result := toRetornoCancelamentoDadosRateio;
    35: Result := toRetornoDesagendamentoDebitoAutomatico;
    40: Result := toRetornoAlteracaoSeuNumero;
    42: Result := toRetornoRecebimentoInstrucaoAlterarDados;
    43: Result := toRetornoAlterarSacadorAvalista;
    51: Result := toRetornoTituloDDAReconhecidoPagador;
    52: Result := toRetornoTituloDDANaoReconhecidoPagador;
    53: Result := toRetornoTituloDDARecusadoCIP;
    98: Result := toRetornoProtestado;
    99: Result := toRetornoRemessaRejeitada;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoBanestes.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                          : Result := '02';
    toRetornoRegistroRecusado                            : Result := '03';
    toRetornoTransferenciaCarteiraEntrada                : Result := '04';
    toRetornoTransferenciaCarteiraBaixa                  : Result := '05';
    toRetornoLiquidado                                   : Result := '06';
    toRetornoRecebimentoInstrucaoConcederDesconto        : Result := '07';
    toRetornoRecebimentoInstrucaoCancelarDesconto        : Result := '08';
    toRetornoBaixado                                     : Result := '09';
    toRetornoTituloEmSer                                 : Result := '11';
    toRetornoAbatimentoConcedido                         : Result := '12';
    toRetornoAbatimentoCancelado                         : Result := '13';
    toRetornoVencimentoAlterado                          : Result := '14';
    toRetornoLiquidadoAposBaixaOuNaoRegistro             : Result := '17';
    toRetornoRecebimentoInstrucaoProtestar               : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto          : Result := '20';
    toRetornoSegundaViaInstrumentoProtesto               : Result := '21';
    toRetornoSegundaViaInstrumentoProtestoCartorio       : Result := '22';
    toRetornoEncaminhadoACartorio                        : Result := '23';
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente : Result := '24';
    toRetornoBaixaPorProtesto                            : Result := '25';
    toRetornoInstrucaoRejeitada                          : Result := '26';
    toRetornoDadosAlterados                              : Result := '27';
    toRetornoDebitoTarifas                               : Result := '28';
    toRetornoOcorrenciasDoSacado                         : Result := '29';
    toRetornoAlteracaoDadosRejeitados                    : Result := '30';
    toRetornoAcertoDadosRateioCredito                    : Result := '33';
    toRetornoCancelamentoDadosRateio                     : Result := '34';
    toRetornoDesagendamentoDebitoAutomatico              : Result := '35';
    toRetornoAlteracaoSeuNumero                          : Result := '40';
    toRetornoRecebimentoInstrucaoAlterarDados            : Result := '42';
    toRetornoAlterarSacadorAvalista                      : Result := '43';
    toRetornoTituloDDAReconhecidoPagador                 : Result := '51';
    toRetornoTituloDDANaoReconhecidoPagador              : Result := '52';
    toRetornoTituloDDARecusadoCIP                        : Result := '53';
    toRetornoProtestado                                  : Result := '98';
    toRetornoRemessaRejeitada                            : Result := '99';
  else
    Result := '02';
  end;
end;

function TACBrBancoBanestes.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of

    //Tabela 1
    {02}toRetornoRegistroConfirmado,{03}toRetornoRegistroRecusado,
    {26}toRetornoInstrucaoRejeitada, {30}toRetornoAlteracaoDadosRejeitados:
    case CodMotivo  of
       01: Result := 'CODIGO DO BANCO INVALIDO';
       02: Result := 'CODIGO DO REGISTRO DETALHE INVALIDO';
       03: Result := 'CODIGO DE SEGMENTO INVALIDO';
       04: Result := 'CODIGO DE MOVIMENTO NÃO PERMITIDO PARA CARTEIRA';
       05: Result := 'CODIGO DE MOVIMENTO INVALIDO';
       06: Result := 'TIPO/NUMERO DE INCRIÇÃO DO CEDENTE INVALIDO';
       07: Result := 'AGENCIA/CONTA/DV INVALIDO';
       08: Result := 'NOSSO NUMERO INVALIDO';
       09: Result := 'NOSSO NUMERO DUPLICADO';
       10: Result := 'CARTEIRA INVALIDA';
       11: Result := 'FORMA DE CADASTRAMENTO DE TITULO INVALIDA';
       12: Result := 'TIPO DE DOCUMENTO INVALIDO';
       13: Result := 'IDENTIFICAÇÃO DA EMISSAO DE BOLETA INVALIDA';
       14: Result := 'IDENTIFICAÇÃO DA DISTRIBUIÇAO DA BOLETA INVALIDA';
       15: Result := 'CARACTERISTICAS DA COBRANÇA INCOMPATIVEIS';
       16: Result := 'DATA DE VENCIMENTO INVALIDA';
       17: Result := 'DATA DE VENCIMENTO ANTERIOR A DATA DE EMISSAO';
       18: Result := 'VENCIMENTO FORA DO PRAZO DE OPERAÇÃO';
       19: Result := 'TITULOS A CARGO DE BANCOS CORRESPODENTES COM VENCIMENTO INFERIOR A XX DIAS';
       20: Result := 'VALOR DO TITULO INVALIDO';
       21: Result := 'ESPECIE DO TITULO INVALIDO';
       22: Result := 'ESPECIE DO TITULO NAO PERMITIDO PARA A CARTEIRA';
       23: Result := 'ACEITE INVALIDO';
       24: Result := 'DATA DA EMISSÃO INVALIDA';
       25: Result := 'DATA DA EMISSÃO POSTERIOR A DATA DE ENTRADA';
       26: Result := 'CODIGO DE JUROS E MORA INVALIDO';
       27: Result := 'VALOR/TAXA JUROS INVALIDO';
       28: Result := 'CODIGO DO DESCONTO INVALIDO';
       29: Result := 'VALOR DO DESCONTO MAIOR OU IGUAL VALOR DO TITULO';
       30: Result := 'DESCONTO A CONCEDER NAO CONFERE';
       31: Result := 'CONCESSÃO DE DESCONTO - JA EXISTE DESCONTO ANTERIOR';
       32: Result := 'VALOR DO IOF INVALIDO';
       33: Result := 'VALOR DO ABATIMENTO INVALIDO';
       34: Result := 'VALOR DO ABATIMENTO MAIOR OU IGUAL AO TITULO';
       35: Result := 'VALOR A CONCEDER NAO CONFERE';
       36: Result := 'CONCESSÃO DE ABATIMENTO - JA EXISTE ABATIMENTO ANTERIOR';
       37: Result := 'CODIGO PARA PROTESTO INVALIDO';
       38: Result := 'PRAZO PARA PROTESTO INVALIDO';
       39: Result := 'PEDIDO DE PROTESTO NAO PERMITIDO PARA O TITULO';
       40: Result := 'TITULO COM ORDEM DE PROTESTO EMITIDA';
       41: Result := 'PEDIDO CANCELAMENTO/SUSTAÇÃO PARA TITULO SEM INSTRUÇÃO DE PROTESTO';
       42: Result := 'CODIGO PARA BAIXA/DEVOLUÇÃO INVALIDO';
       43: Result := 'PRAZO PARA BAIXA/DEVOLUÇÃO INVALIDO';
       44: Result := 'CODIGO DA MOEDA INVALIDO';
       45: Result := 'NOME DO SACADO NAO INFORMADO';
       46: Result := 'TIPO/NUMERO DE INSCRIÇÃO DO SACADO INVALIDO';
       47: Result := 'ENDEREÇO DO SACADO NÃO INFORMADO';
       48: Result := 'CEP INVALIDO';
       49: Result := 'CEP SEM PRAÇA DE COBRANÇA (NAO LOCALIZADO)';
       50: Result := 'CEP REFERENTE A UM BANCO CORRESPODENTE';
       51: Result := 'CEP INCOMPATIVEL COM A UNIDADE DA FEDERAÇÃO';
       52: Result := 'UNIDADE DA FEDERAÇÃO INVALIDA';
       53: Result := 'TIPO/NUMERO INSCRIÇÃO DO SACADOR/AVALISTA INVALIDO';
       54: Result := 'SACADOR AVALISTA NAO INFORMADO';
       55: Result := 'NOSSO NUMERO NO BANCO CORRESPODENTE NAO INFORMADO';
       56: Result := 'CODIGO DO BANCO CORRESPODENTE NAO INFORMADO';
       57: Result := 'CODIGO DA MULTA INVALIDO';
       58: Result := 'DATA DA MULTA INVALIDA';
       59: Result := 'VALOR PERCENTUAL DA MULTA INVALIDA';
       60: Result := 'MOVIMENTO PARA TITULO NAO CADASTRADO';
       61: Result := 'ALTERAÇÃO DA AGÊNCIA COBRADORA DV INVALIDO';
       62: Result := 'TIPO IMPRESSÃO INVALIDA';
       63: Result := 'ENTRADA TITULO JA CADASTRADO';
       64: Result := 'NUMERO LINHA INVALIDO';
       65: Result := 'CODIGO BANCO PARA DEBITO INVALIDO';
       66: Result := 'AGENCIA / CC /DV PARA DEBITO INVALIDO';
       67: Result := 'DADOS PARA DEBITO INCOMPATIVEL COM A IDENTIFICACAO DA EMISSAO DO BOLETO';
       68: Result := 'DEBITO AUTOMATICO AGENDADO';
       69: Result := 'DÉBITO NÃO AGENDADO - ERRO NOS DADOS DA REMESSA';
       70: Result := 'DÉBITO NÃO AGENDADO - SACADO NÃO CONSTA DO CADASTRO DE AUTORIZANTE';
       71: Result := 'DÉBITO NÃO AGENDADO - CEDENTE NÃO AUTORIZADO PELO SACADO';
       72: Result := 'DÉBITO NÃO AGENDADO - CEDENTE NÃO PARTICIPA DA MODALIDADE DÉBITO AUTOMÁTICO';
       73: Result := 'DÉBITO NÃO AGENDADO - CÓDIGO DE MOEDA DIFERENTE DE REAL (R$)';
       74: Result := 'DÉBITO NÃO AGENDADO - DATA VENCIMENTO INVÁLIDA';
       75: Result := 'DÉBITO NÃO AGENDADO, CONFORME SEU PEDIDO, TÍTULO NÃO REGISTRADO';
       76: Result := 'DÉBITO NÃO AGENDADO, TIPO/NUM. INSCRIÇÃO DO DEBITADO, INVÁLIDO';
       77: Result := 'TRANSFERÊNCIA PARA DESCONTO NÃO PERMITIDA PARA A CARTEIRA DO TÍTULO';
       78: Result := 'DATA INFERIOR OU IGUAL AO VENCIMENTO PARA DÉBITO AUTOMÁTICO';
       79: Result := 'DATA JUROS DE MORA INVÁLIDO';
       80: Result := 'DATA DO DESCONTO INVÁLIDA';
       81: Result := 'TENTATIVAS DE DÉBITO ESGOTADAS - BAIXADO';
       82: Result := 'TENTATIVAS DE DÉBITO ESGOTADAS - PENDENTE';
       83: Result := 'LIMITE EXCEDIDO';
       84: Result := 'NÚMERO AUTORIZAÇÃO INEXISTENTE';
       85: Result := 'TÍTULO COM PAGAMENTO VINCULADO';
       86: Result := 'SEU NÚMERO INVÁLIDO';
       92: Result := 'SACADO ELETRÔNICO - DDA';
       98: Result := 'ERRO NA FORMAÇÃO DO CARNÊ';
    else
       Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
    end;

    toRetornoTarifaOcorrencias, {28}toRetornoDebitoTarifas:
    case CodMotivo of
       01: Result := 'TARIFAS DE EXTRATO DE POSIÇÃO';
       02: Result := 'TAFIFA DE MANUTENÇÃO DE TITULO VENCIDA';
       03: Result := 'TARIFA DE SUSTAÇÃO';
       04: Result := 'TARIFA DE PROTESTO';
       05: Result := 'TARIFA DE OUTRAS INSTRUÇOES';
       06: Result := 'TARIFA DE OUTRAS OCORRENCIAS';
       07: Result := 'TARIFA DE ENVIO DE DUPLICATA AO SACADO';
       08: Result := 'CUSTAS DE PROTESTO';
       09: Result := 'CUSTAS DE SUSTAÇÃO DE PROTESTO';
       10: Result := 'CUSTAS DE CARTÓRIO DISTRIBUIDOR';
       11: Result := 'CUSTAS DE EDITAL';
       12: Result := 'TARIFA SOBRE DEVOLUCAO E TITULO VENCIDO';
       13: Result := 'TARIFA SOBRE REGISTRO COBRADO NA BAIXA';
       14: Result := 'TARIFA SOBRE REAPRESENTAÇÃO AUTOMATICA';
       15: Result := 'TARIFA SOBRE RATEIO DE CRÉDITO';
       16: Result := 'TARIFA SOBRE INFORMAÇÃO VIA FAX';
       17: Result := 'TARIFA SOBRE PRORROGAÇÃO DE VENCIMENTO';
       18: Result := 'TARIFA SOBRE ALTERAÇÃO DE ABATIMENTO/DESCONTO';
       19: Result := 'TARIFA SOBRE ARQUIVO MENSAL';
       20: Result := 'TARIFA EMISSÃO BOLETO EMITIDO PELO BANCO';

    else
       Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
    end;

    {06}toRetornoLiquidado, {09}toRetornoBaixado, {17}toRetornoLiquidadoAposBaixaOuNaoRegistro:
    case CodMotivo of
       01: Result := 'POR SALDO';
       02: Result := 'POR CONTA';
       03: Result := 'NO PROPRIO BANCO';
       04: Result := 'COMPENSAÇÃO ELETRONICA';
       05: Result := 'COMPENSAÇÃO CONVENCIONAL';
       06: Result := 'POR MEIO ELETRÔNICO';
       07: Result := 'DEPOIS DE FERIADO LOCAL';
       08: Result := 'EM CARTÓRIO';
       09: Result := 'COMANDADA BANCO';
       10: Result := 'COMANDADA CLIENTE ARQUIVO';
       11: Result := 'COMANDADA CLIENTE ONLINE';
       12: Result := 'DECURSO PRAZO CLIENTE';
       13: Result := 'DECURSO PRAZO BANCO';
       14: Result := 'PROTESTADO';
       15: Result := 'TITULO EXCLUIDO';
    else
       Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
    end;

    {99}toRetornoOutrasOcorrencias:
    case CodMotivo of
      01: Result := 'REMESSA EM DUPLICIDADE';
      02: Result := 'ERRO NA SEQUÊNCIA DO REGISTRO';
      03: Result := 'CÓDIGO DO BANCO INVÁLIDO';
      04: Result := 'CEDENTE NÃO CADASTRADO';
      05: Result := 'REGISTRO NÃO É HEADER';
      06: Result := 'ARQUIVO NAO É REMESSA';
      07: Result := 'SERVIÇO NÃO É COBRANCA';
      08: Result := 'CONTA CORRENTE DIFERENTE DO REGISTRO HEADER';
      09: Result := 'CÓDIGO DE OCORRÊNCIA INVÁLIDO';
      10: Result := 'ERRO NA SEQUÊNCIA DO LOTE';
      11: Result := 'NÚMERO DO LOTE DO REGISTRO DETALHES DIFERE DO REGISTRO DE HEADER';
      12: Result := 'FALTA REGISTRO DE TRAILLER DO ARQUIVO';
      13: Result := 'NÚMERO DO LOTE DO REGISTRO TRAILLER DIFERE DO REGISTRO DE HEADER';
      14: Result := 'ERRO NA QUANTIDADE DE REGISTRO DO LOTE';
      15: Result := 'NÚMERO DO LOTE DO REGISTRO TRAILLER INVÁLIDO';
      16: Result := 'ERRO NA QUANTIDADE DE LOTES DO ARQUIVO';
      17: Result := 'CÓDIGO DO SEGMENTO FORA DE ORDEM';
      18: Result := 'CÓDIGO DO REGISTRO FORA DE ORDEM';
      19: Result := 'CÓDIGO DO REGISTRO INVÁLIDO';
      99: Result := 'OUTROS MOTIVOS';
    end;
  end;

  Result := ACBrSTr(Result);
end;

end.
