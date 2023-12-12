{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou,marcelo.hgv                     }
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

unit ACBrBancoSicredi;

interface

uses
  Classes, SysUtils, Contnrs,
  ACBrBoleto, ACBrBoletoConversao;

const
  CInstrucaoPagamentoCooperativa = 'Pagável preferencialmente nas cooperativas de crédito do %s';

type
  { TACBrBancoSicredi }

  TACBrBancoSicredi = class(TACBrBancoClass)
  protected
    function GetLocalPagamento: String; override;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function DefineDataDesconto(const ACBrTitulo: TACBrTitulo; AFormat: String = 'ddmmyyyy'): String; override;

    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa:TStringList): String;  override;
    procedure LerRetorno240(ARetorno: TStringList); override;

    function CalcularNomeArquivoRemessa : String; override;

    function TipoDescontoToString(const AValue: TACBrTipoDesconto): String; override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo:String): String; override;

    function CompOcorrenciaOutrosDadosToDescricao(const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String; override;
    function CompOcorrenciaOutrosDadosToCodigo(const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String; override;


    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrValidador;

{ TACBrBancoSicredi }

constructor TACBrBancoSicredi.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 10;
   fpNome                  := 'Sicredi';
   fpNumero                := 748;
   fpTamanhoMaximoNossoNum := 5;
   fpTamanhoAgencia        := 4;
   fpTamanhoConta          := 5;
   fpTamanhoCarteira       := 1;
   fpCodigosMoraAceitos    := 'AB0123';
   fpCodigosGeracaoAceitos := '23456789';
   fpLayoutVersaoArquivo   := 81;
   fpLayoutVersaoLote      := 40;
end;

function TACBrBancoSicredi.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Modulo.CalculoPadrao;
   Modulo.Documento := ACBrTitulo.ACBrBoleto.Cedente.Agencia +
                       PadLeft(ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito, 2, '0') +
                       PadLeft(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente, 5, '0') +
                       FormatDateTime('yy',ACBrTitulo.DataDocumento) +
                       ACBrTitulo.CodigoGeracao + RightStr(ACBrTitulo.NossoNumero,5);
   Modulo.Calcular;

   if (Modulo.DigitoFinal > 9) then
      Result := '0'
   else
      Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoSicredi.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras, CampoLivre, Modalidade:String;
  DigitoNum: Integer;
begin
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

	    Modalidade := IfThen(Trim(Cedente.Modalidade) = '', '1', Copy(Trim(Cedente.Modalidade),1,1));

      { Monta o campo livre }
      CampoLivre :=   Modalidade                                      + { 1-Com registro ou 3-Sem registro. Por enquanto vou deixar 1 mais tenho que tratar menhor essa informação }
                      '1'                                             + { 1-Carteira simples }
                      OnlyNumber(MontarCampoNossoNumero(ACBrTitulo))  +
                      PadLeft(OnlyNumber(Cedente.Agencia),4,'0')      + { Código agência (cooperativa) }
                      PadLeft(Cedente.AgenciaDigito,2,'0')            + { Dígito da agência (posto da cooperativa) }
                      PadLeft(OnlyNumber(Cedente.CodigoCedente),5,'0')+ { Código cedente }  //  Ver manual página 86 - CNAB240 ou 51 - CNAB400
                      '1'                                             + { Filler - zero. Obs: Será 1 quando o valor do documento for diferente se zero }
                      '0';                                              { Filler - zero }

      { Calcula o dígito do campo livre }
      Modulo.CalculoPadrao;
      Modulo.MultiplicadorFinal := 9;
      Modulo.Documento := CampoLivre;
      Modulo.Calcular;
      CampoLivre := CampoLivre + IntToStr(Modulo.DigitoFinal);

      { Monta o código de barras }
      CodigoBarras := IntToStr( Numero )                                     + { Código do banco 748 }
                      '9'                                                    + { Fixo '9' }
                      FatorVencimento                                        + { Fator de vencimento, não obrigatório }
                      IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10)  + { valor do documento }
                      CampoLivre;                                              { Campo Livre }



      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
      DigitoNum := StrToIntDef(DigitoCodBarras,0);

      if (DigitoNum = 0) or (DigitoNum > 9) then
         DigitoCodBarras:= '1';
   end;

   Result:= IntToStr(Numero) + '9' + DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoSicredi.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
begin
  if ( (ACBrBanco.ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite) and ( (Trim(ACBrTitulo.NossoNumero) = '') or (Trim(ACBrTitulo.NossoNumero) = '00000') ) ) then
    Result := ''
  else
    Result:= FormatDateTime('yy',ACBrTitulo.DataDocumento) + '/' +
             ACBrTitulo.CodigoGeracao + RightStr(ACBrTitulo.NossoNumero,5) + '-' +
             CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoSicredi.MontarCampoCodigoCedente (const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '.' +
             PadLeft(ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito,2,'0')+ '.' +
             PadLeft(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente,5,'0');
end;

procedure TACBrBancoSicredi.GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList);
var wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                    + // ID do Registro
               '1'                                    + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                              + // Literal de Remessa
               '01'                                   + // Código do Tipo de Serviço
               PadRight( 'COBRANCA', 15 )             + // Descrição do tipo de serviço
               PadLeft( CodigoCedente, 5, '0')        + // Codigo da Empresa no Banco
               PadLeft( OnlyNumber(CNPJCPF), 14, '0') + // CNPJ do Cedente
               Space(31)                              + // Fillers - Branco
               '748'                                  + // Número do banco
               PadRight('SICREDI', 15)                + // Código e Nome do Banco(237 - Bradesco)
               FormatDateTime('yyyymmdd',Now)         + // Data de geração do arquivo
               Space(8)                               + // Filler - Brancos
               IntToStrZero(NumeroRemessa,7)          + // Nr. Sequencial de Remessa + brancos
               Space(273)                             + // Filler - Brancos
               '2.00'                                 + // Versão do sistema
               IntToStrZero(1,6);                       // Nr. Sequencial de Remessa + brancos + Contador

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoSicredi.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  wNossoNumeroCompleto, CodProtesto, DiasProtesto, CodNegativacao, DiasNegativacao: String;
  TipoSacado, AceiteStr, wLinha, Ocorrencia, TpDesconto, CompOcorrenciaOutrosDados : String;
  TipoBoleto, wModalidade: Char;
  TextoRegInfo: String;
  ANumeroDocumento: String;
  LHibrido : string;
begin

   with ACBrTitulo do
   begin
      wNossoNumeroCompleto := OnlyNumber(MontarCampoNossoNumero(ACBrTitulo));
      ANumeroDocumento := PadRight(IfThen(SeuNumero <> '', SeuNumero, NumeroDocumento), 10, ' ');

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : Ocorrencia := '06'; {Alteração de vencimento}
         toRemessaProtestar                      : Ocorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '18'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : Ocorrencia := '19'; {Sustar protesto e manter na carteira}
         toRemessaOutrasOcorrencias              : Ocorrencia := '31'; {Alteração de Outros Dados}
         toRemessaNegativacaoSerasa              : Ocorrencia := '45'; {Negativar Serasa}
         toRemessaExcluirNegativacaoSerasa       : Ocorrencia := '75'; {Excluir Negativação Serasa}
         toRemessaExcluirNegativacaoSerasaBaixar : Ocorrencia := '76'; {Excluir Negativação Serasa e Baixar}
      else
         Ocorrencia := '01';                                          {Remessa}
      end;

      if(OcorrenciaOriginal.Tipo = toRemessaOutrasOcorrencias)Then
      begin
        CompOcorrenciaOutrosDados := CompOcorrenciaOutrosDadosToCodigo(OcorrenciaOriginal.ComplementoOutrosDados);
      end;

      {Pegando Tipo de Boleto}
      if (ACBrBoleto.Cedente.ResponEmissao <> tbCliEmite) or 
         (CarteiraEnvio <> tceCedente) then
        TipoBoleto := 'A'
      else
        TipoBoleto := 'B';       

      {Pegando campo Protesto}
      if (DataProtesto > 0) and (DataProtesto >= Vencimento + 3) then // mínimo de 3 dias de protesto
      begin
        CodProtesto := '06';
        DiasProtesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento),2);
      end
      else
      begin
        CodProtesto := '00';
        DiasProtesto := '00';
      end;

      {Pegando campo Negativação Serasa}
      if (DataNegativacao > 0) and (DataNegativacao >= Vencimento + 3) then // mínimo de 3 dias de negativacaoserasa
      begin
        CodNegativacao := '06';
        DiasNegativacao := IntToStrZero(DaysBetween(DataNegativacao,Vencimento),2);
      end
      else
      begin
        CodNegativacao := '00';
        DiasNegativacao := '00';
      end;

      {Pegando Tipo de Sacado}
      if Length(OnlyNumber(Sacado.CNPJCPF)) > 11 then
         TipoSacado:= '2'
      else
         TipoSacado:= '1';

      { Pegando o tipo de EspecieDoc }
      if EspecieDoc = 'DMI' then
         EspecieDoc   := 'A'
      else if EspecieDoc = 'DR' then
         EspecieDoc   := 'B'
      else if EspecieDoc = 'NP' then
         EspecieDoc   := 'C'
      else if EspecieDoc = 'NR' then
         EspecieDoc   := 'D'
      else if EspecieDoc = 'NS' then
         EspecieDoc   := 'E'
      else if EspecieDoc = 'RC' then
         EspecieDoc   := 'G'
      else if EspecieDoc = 'LC' then
         EspecieDoc   := 'H'
      else if EspecieDoc = 'ND' then
         EspecieDoc   := 'I'
      else if EspecieDoc = 'DSI' then
         EspecieDoc   := 'J'
      else if EspecieDoc = 'OS' then
         EspecieDoc   := 'K'
      else
         EspecieDoc := 'A';

      {Pegando o Aceite}
      case Aceite of
        atSim: AceiteStr := 'S';
        atNao: AceiteStr := 'N';
      else
         AceiteStr := 'N';
      end;

      if StrToIntDef(ACBrBoleto.Cedente.Modalidade,1) = 1 then
         wModalidade := 'A'
      else
         wModalidade := 'C'; 

     if (CodigoMora <> 'A') and (CodigoMora <> 'B') then
       CodigoMora := 'A';

     TpDesconto := TipoDescontoToString(TipoDesconto);

     LHibrido := IfThen(NaoEstaVazio(ACBrBoleto.Cedente.PIX.Chave),'H',' ');

      with ACBrBoleto do
      begin
         wLinha:= '1'                                                                   +  // 001 a 001 - Identificação do registro detalhe
                  wModalidade                                                           +  // 002 a 002 - Tipo de cobrança ("A" - Registrada  e "C" Sem Regsitro)
                  ifthen(wModalidade = 'A', 'A', ' ')                                   +  // 003 a 003 - Tipo de carteira = "A" Simples
                  IfThen(TipoImpressao = tipCarne, 'B', 'A')                            +  // 004 a 004 - Tipo de impressão = "A" Normal "B" Carnê //--Anderson
                  ' '                                                                   +  // 005 a 005 - Filler - Brancos
                  LHibrido                                                              +  // 006 a 006 - Tipo de Boleto: H - Híbrido
                  Space(10)                                                             +  // 007 a 016 - Filler - Brancos
                  'A'                                                                   +  // 017 a 017 - Tipo de moeda = "A" Real
                  TpDesconto                                                            +  // 018 a 018 - Tipo de desconto: "A" Valor "B" percentual
                  trim(CodigoMora)                                                      +  // 019 a 019 - Tipo de juro: "A" Valor "B" percentual
                  Space(28)                                                             +  // 020 a 047 - Filler - Brancos
                  PadLeft(wNossoNumeroCompleto,9,'0');                                     // 048 a 056 - Nosso número sem edição YYXNNNNND - YY=Ano, X-Emissao, NNNNN-Sequência, D-Dígito

         if wModalidade = 'A' then
            wLinha:= wLinha +
                     Space(6)                                                           +  // 057 a 062 - Filler - Brancos
                     FormatDateTime( 'yyyymmdd', date)                                  +  // 063 a 070 - Data da instrução
                     IfThen(Ocorrencia = '31', CompOcorrenciaOutrosDados, Space(1))     +  // 071 a 071 - Campo alterado, quando instrução "31" Conforme tabela de instruções
                     IfThen(CarteiraEnvio = tceBanco, 'S', 'N')                         +  // 072 a 072 - Postagem do título = "S" Para postar o título "N" Não postar e remeter para o cedente
                     Space(1)                                                           +  // 073 a 073 - Filler Brancos
                     TipoBoleto                                                         +  // 074 a 074 - Emissão do bloqueto = "A" Impressão pelo SICREDI "B" Impressão pelo Cedente
                     IfThen((TipoImpressao = tipCarne) and (Parcela > 0), 
                            PadLeft(IntToStr(Parcela),2,'0'), '  ')                     +  // 075 a 076 - Número da parcela do carnê --Anderson
                     IfThen((TipoImpressao = tipCarne) and (TotalParcelas > 0),
                            PadLeft(IntToStr(TotalParcelas),2,'0'), '  ')                  // 077 a 078 - Número total de parcelas do carnê -- Anderson
         else
            wLinha:= wLinha +
                     Space(1)                                                           +  // 057 a 057 - Filler - Brancos
                     'B'                                                                +  // 058 a 058 - Tipo Impressão (Completa)
                     Space(13)                                                          +  // 059 a 071 - Filler - Brancos
                     IfThen(CarteiraEnvio = tceBanco, 'S', 'N')                         +  // 072 a 072 - Postagem do título = "S" Para postar o título "N" Não postar e remeter para o cedente
                     Space(2)                                                           +  // 073 a 074 - Filler Brancos
                     IfThen((TipoImpressao = tipCarne) and (Parcela > 0),
                             PadLeft(IntToStr(Parcela),2,'0'), '  ')                    +  // 075 a 076 - Número da parcela do carnê --Anderson
                     IfThen((TipoImpressao = tipCarne) and (TotalParcelas > 0),
                            PadLeft(IntToStr(TotalParcelas),2,'0'), '  ');                 // 077 a 078 - Número total de parcelas do carnê -- Anderson

         wLinha:= wLinha +
                  Space(4)                                                              +  // 079 a 082 - Filler - Brancos
                  IntToStrZero(round(ValorDescontoAntDia * 100), 10)                    +  // 083 a 092 - Valor de desconto por dia de antecipação
                  IntToStrZero( round( PercentualMulta * 100 ), 4)                      +  // 093 a 096 - % multa por pagamento em atraso
                  Space(12)                                                             +  // 097 a 108 - Filler - Brancos
                  Ocorrencia                                                            +  // 109 a 110 - Instrução = "01" Cadastro de título ... ---Anderson
                  ANumeroDocumento                                                      +  // 111 a 120 - Seu número
                  FormatDateTime( 'ddmmyy', Vencimento)                                 +  // 121 a 126 - Data de vencimento
                  IntToStrZero( Round( ValorDocumento * 100 ), 13)                      +  // 127 a 139 - Valor do título
                  Space(9)                                                              +  // 140 a 148 - Filler - Brancos
                  EspecieDoc                                                            +  // 149 a 149 - Espécie de documento
                  IfThen(wModalidade = 'A', AceiteStr, ' ')                             +  // 150 a 150 - Aceite do título
                  FormatDateTime( 'ddmmyy', DataDocumento );                               // 151 a 156 - Data de emissão

         if wModalidade = 'A' then
            wLinha:= wLinha +
                     CodProtesto                                                        +  // 157 a 158 - Instrução de protesto automático = "00" Não protestar "06" Protestar automaticamente
                     DiasProtesto                                                          // 159 a 160 - Número de dias para protesto automático
         else
            wLinha:= wLinha + Space(4);                                                    // 157 a 160 - Filler Brancos

         wLinha:= wLinha +
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13)                       +  // 161 a 173 - Valor/% de juros por dia de atraso
                  IfThen(DataDesconto < EncodeDate(2000,01,01),'000000',
                         FormatDateTime( 'ddmmyy', DataDesconto))                       +  // 174 a 179 - Data limite para concessão de desconto
                  IntToStrZero( round( ValorDesconto * 100 ), 13);                         // 180 a 192 - Valor/% do desconto

         if wModalidade = 'A' then
            wLinha:= wLinha +
                     CodNegativacao                                                     +  // 193 a 194 - Instrução de negativação automática = "00" Não negativar automaticamente "06" Negativar automaticamente
                     DiasNegativacao                                                       // 195 a 196 - Número de dias para negativação automática
         else
            wLinha:= wLinha + Space(4);                                                    // 193 a 196 - Filler Brancos

         if wModalidade = 'A' then
            wLinha:= wLinha +
                     PadRight('', 9, '0')                                               +  // 197 a 205 - Filler - Zeros
                     IntToStrZero( round( ValorAbatimento * 100 ), 13)                     // 206 a 218 - Valor do abatimento
         else
            wLinha:= wLinha + PadRight('', 22, '0');                                       // 197 a 218 - Filler Zeros


         wLinha:= wLinha +
                  TipoSacado                                                            +  // 219 a 219 - Tipo de pessoa do sacado: PF ou PJ = "1" Pessoa Física "2" Pessoa Jurídica
                  IfThen(wModalidade = 'A', '0', ' ')                                   +  // 220 a 220 - Filler - (Cob. Registrada = '0', Cob. Sem Registro = ' ')
                  PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')                            +  // 221 a 234 - CIC/CGC do sacado
                  PadRight( TiraAcentos(Sacado.NomeSacado), 40, ' ');                                   // 235 a 274 - Nome do sacado

         if wModalidade = 'A' then
            wLinha:= wLinha +
                     PadRight(TiraAcentos(Sacado.Logradouro + ',' + Sacado.Numero + ',' +
                           Sacado.Bairro + ',' + Sacado.Cidade + ',' +
                           Sacado.UF), 40)                                               +  // 275 a 314 - Endereço do sacado
                     PadRight('', 5, '0')                                               +  // 315 a 319 - Código do sacado na cooperativa cedente (utilizar zeros)
                     PadRight('', 6, '0')                                               +  // 320 a 325 - Filler - Zeros
                     Space(1)                                                           +  // 326 a 326 - Filler - Brancos
                     PadRight( OnlyNumber(Sacado.CEP), 8 )                              +  // 327 a 334 - CEP do sacado
                     PadRight('', 5, '0')                                               +  // 335 a 339 - Código do sacado junto ao cliente (zeros quando inexistente)
                     PadRight(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 14, ' ')       +  // 340 a 353 - CIC/CGC do sacador avalista
                     PadRight(TiraAcentos(Sacado.Avalista), 41, ' ')                                    // 354 a 394 - Nome do sacador avalista ---Anderson
         else
            wLinha:= wLinha +
                     PadRight(TiraAcentos(Sacado.Logradouro + ',' + Sacado.Numero + ',' +
                           Sacado.Bairro), 40)                                           +  // 275 a 314 - Endereço do sacado
                     PadRight('', 5, '0')                                               +  // 315 a 319 - Código do sacado na cooperativa beneficiário (zeros quando inexistente)
                     PadRight('', 6, '0')                                               +  // 320 a 325 - Filler - Zeros
                     Space(1)                                                           +  // 326 a 326 - Filler - Brancos
                     PadRight( OnlyNumber(Sacado.CEP), 8 )                              +  // 327 a 334 - CEP do sacado
                     PadRight(TiraAcentos(Sacado.Cidade), 25  )                                     +  // 335 a 359 - Cidade do sacado
                     PadRight( Sacado.UF, 2  )                                          +  // 360 a 361 - UF do sacado
                     PadRight('', 5, '0')                                               +  // 362 a 366 - Código do sacado junto ao cliente (zeros quando inexistente)
                     PadRight('', 28, ' ');                                                // 367 a 394 - Filler - Brancos


         wLinha:= wLinha + IntToStrZero(aRemessa.Count + 1, 6 );  // 395 a 400 - Número sequencial do registro

         aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);

         {8.3 Registro Mensagem}
         if (Mensagem.Count > 0) and (OcorrenciaOriginal.Tipo <> toRemessaBaixar) then
         begin
           TextoRegInfo := PadRight(Mensagem[0], 80);
           if Mensagem.Count > 1 then
             TextoRegInfo := TextoRegInfo + PadRight(Mensagem[1], 80)
           else
             TextoRegInfo := TextoRegInfo + PadRight('', 80);

           if Mensagem.Count > 2 then
             TextoRegInfo := TextoRegInfo + PadRight(Mensagem[2], 80)
           else
             TextoRegInfo := TextoRegInfo + PadRight('', 80);

           if Mensagem.Count > 3 then
             TextoRegInfo := TextoRegInfo + PadRight(Mensagem[3], 80)
           else
             TextoRegInfo := TextoRegInfo + PadRight('', 80);

           wLinha:=
             '2'                                                  + // 001 - 001 Tipo do Registro Detalhe
             PadRight('', 11, ' ')                                + // 002 - 012 Filler - Deixar em Branco
             PadLeft(wNossoNumeroCompleto,9,'0')                  + // 013 - 021 Nosso Número
             TextoRegInfo                                         +  //022 - 341 1,2,3,4 instrucao impressa no boleto
             ANumeroDocumento                                     + // 342 - 351 Seu Número
             PadRight('', 43, ' ');                                 // 352 - 394 Filler - Deixar em Branco

           wLinha:= wLinha + IntToStrZero(aRemessa.Count + 1, 6 );  // 395 - 400 Número sequencial do registro

           ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);

         end;

         { Registro Informativo }
         if Informativo.Count > 0 then
         begin
           TextoRegInfo := PadLeft('1', 2, '0')          + // Numero da linha do informativo
                           PadRight(Informativo[0], 80);   // Texto da linha do informativo

           if Informativo.Count >= 2 then
             TextoRegInfo:= TextoRegInfo + PadLeft('2', 2, '0') +
                            PadRight(Informativo[1], 80)
           else
             TextoRegInfo:= TextoRegInfo + '02' + PadRight('', 80);

           if Informativo.Count >= 3 then
             TextoRegInfo:= TextoRegInfo + PadLeft('3', 2, '0') +
                            PadRight(Informativo[2], 80)
           else
             TextoRegInfo:= TextoRegInfo + '03' + PadRight('', 80);

           if Informativo.Count >= 4 then
             TextoRegInfo:= TextoRegInfo + PadLeft('4', 2, '0') +
                            PadRight(Informativo[3], 80)
           else
             TextoRegInfo:= TextoRegInfo + '04' + PadRight('', 80);

           wLinha:=  '5'                                                         + // 001 a 001 - Identificação do registro Informativo
                     'E'                                                         + // 002 a 002 - Tipo de informativo
                     PadLeft( ACBrBanco.ACBrBoleto.Cedente.CodigoCedente, 5, '0')+ // 003 a 004 - Codigo do Cedente
                     ifthen(wModalidade = 'A', ANumeroDocumento,
                            padLeft(wNossoNumeroCompleto,10,'0'))                + // 008 a 017 - Seu numero
                     Space(1)                                                    + // 018 a 018 - Filler
                     wModalidade                                                 + // 019 a 019 - "A"-Com registro  "C"-Sem registro
                     TextoRegInfo                                                + // 020 a 347
                     Space(47)                                                   + // 348 a 394 - Filler
                     IntToStrZero( ARemessa.Count + 1, 6);                         // 395 a 400 - Número sequencial do registro
           ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);


           if Informativo.Count >= 5 then
           begin
             TextoRegInfo:= PadLeft('5', 2, '0') + PadRight(Informativo[4], 80);

             if Informativo.Count >= 6 then
               TextoRegInfo:= TextoRegInfo + PadLeft('6', 2, '0') +
                              PadRight(Informativo[5], 80)
             else
               TextoRegInfo:= TextoRegInfo + '06' + PadRight('', 80);

             if Informativo.Count >= 7 then
               TextoRegInfo:= TextoRegInfo + PadLeft('7', 2, '0') +
                              PadRight(Informativo[6], 80)
             else
               TextoRegInfo:= TextoRegInfo + '07' + PadRight('', 80);

             if Informativo.Count >= 8 then
               TextoRegInfo:= TextoRegInfo + PadLeft('8', 2, '0') +
                              PadRight(Informativo[7], 80)
             else
               TextoRegInfo:= TextoRegInfo + '08' + PadRight('', 80);

             wLinha:=  '5'                                                         + // 001 a 001 - Identificação do registro Informativo
                       'E'                                                         + // 002 a 002 - Tipo de informativo
                       PadLeft( ACBrBanco.ACBrBoleto.Cedente.CodigoCedente, 5, '0')+ // 003 a 004 - Codigo do Cedente
                       ifthen(wModalidade = 'A', ANumeroDocumento,
                              padLeft(wNossoNumeroCompleto, 10,'0'))                                  +// 008 a 017 - Seu numero
                       Space(1)                                                    + // 018 a 018 - Filler
                       'A'                                                         + // 019 a 019 - "A"-Com registro  "C"-Sem registro
                       TextoRegInfo                                                + // 020 a 347
                       Space(47)                                                   + // 348 a 394 - Filler
                       IntToStrZero( ARemessa.Count + 1, 6);                         // 395 a 400 - Número sequencial do registro
             ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
          end;
        end;

        if (Trim(Sacado.SacadoAvalista.NomeAvalista) <> EmptyStr) and (Trim(Sacado.SacadoAvalista.CNPJCPF)<> EmptyStr)
          and (Trim(Sacado.SacadoAvalista.Logradouro)<> EmptyStr) and (Trim(Sacado.SacadoAvalista.Cep)    <> EmptyStr)
          and (Trim(Sacado.SacadoAvalista.Uf)        <> EmptyStr) and (Trim(Sacado.SacadoAvalista.Cidade) <> EmptyStr) then
        begin
          wLinha:=  '6'                                                            + // 001 a 001 - Identificação do registro Informativo (6)
                    PadLeft(wNossoNumeroCompleto,15,' ')                           + // 002 a 016 - Nosso número Sicredi
                    ANumeroDocumento                                               + // 017 a 026 - Seu número
                    PadRight('', 5, '0')                                           + // 027 a 031 - Código do pagador junto ao cliente
                    PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 14, '0')    + // 032 a 045 - CPF/CNPJ do Sacador Avalista ( Obrigatório )
                    PadRight( TiraAcentos( Sacado.SacadoAvalista.NomeAvalista
                                          ), 41, ' ')                              + // 046 a 086 - Nome do Sacador Avalista ( Obrigatório )
                    PadRight( TiraAcentos( Sacado.SacadoAvalista.Logradouro  + ',' +
                                          Sacado.SacadoAvalista.Numero      + ','  +
                                          Sacado.SacadoAvalista.Bairro      + ','  +
                                          Sacado.SacadoAvalista.Complemento
                                        ), 45, ' ')                                + // 087 a 131 - Endereço (Obrigatório)
                    PadRight( TiraAcentos( Sacado.SacadoAvalista.Cidade ), 20, ' ')+ // 132 a 151 - Cidade
                    PadRight(Sacado.SacadoAvalista.Cep, 8, ' ')                    + // 152 a 159 - CEP    ( Obrigatório )
                    PadRight(Sacado.SacadoAvalista.Uf, 2)                          + // 160 a 161 - Estado ( Obrigatório )
                    Space(233)                                                     + // 162 a 394 - Filler ( Deixar em Branco (sem preenchimento) )
                    IntToStrZero( ARemessa.Count + 1, 6);                            // 395 a 400 - Número sequencial do registro
          ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
        end;

        if (ValorDesconto2 > 0) and (DataDesconto2 > 0) then
        begin
          wLinha := '7'                                                            + // 001 a 001 - Identificação do registro detalhe (7)
                    PadLeft(wNossoNumeroCompleto, 15, ' ')                         + // 002 a 016 - Nosso número Sicredi
                    ANumeroDocumento                                               + // 017 a 026 - Seu número
                    PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0')                   + // 027 a 040 - CPF/CNPJ do pagador
                    PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 14, '0')    + // 041 a 054 - CPF/CNPJ do Sacador Avalista
                    IfThen(DataDesconto2 < EncodeDate(2000, 01, 01), '000000',
                         FormatDateTime('ddmmyy', DataDesconto2))                  + // 055 a 060 - Data limite para desconto 2
                    IntToStrZero( Round( ValorDesconto2 * 100 ), 13)               + // 061 a 073 - Valor do desconto 2
                    '000000'                                                       + // 074 a 079 - Data limite para desconto 3
                    '0000000000000'                                                + // 080 a 092 - Valor do desconto 3
                    Space(302)                                                     + // 093 a 394 - Filler ( Deixar em Branco (sem preenchimento) )
                    IntToStrZero( ARemessa.Count + 1, 6);                            // 395 a 400 - Número sequencial do registro
          ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
        end;

        {8.7 Registro Híbrido}
        if (NaoEstaVazio(ACBrBoleto.Cedente.PIX.Chave)) then
        begin
          wLinha := '8'                                                            + // 001 a 001 - Identificação do registro detalhe (7)
                    PadRight(wNossoNumeroCompleto, 15, ' ')                        + // 002 a 016 - Nosso número Sicredi
                    ' '                                                            + // 017 a 017 - Filler ( Deixar em Branco (sem preenchimento) )
                    'H'                                                            + // 018 a 018 - Híbrido
                    Space(12)                                                      + // 019 a 030 - Filler ( Deixar em Branco (sem preenchimento) )
                    ANumeroDocumento                                               + // 031 a 040 - Seu número
                    PadLeft(QrCode.txId, 35, ' ')                                  + // 041 a 075 - TXID
                    Space(319)                                                     + // 076 a 394 - Filler ( Deixar em Branco (sem preenchimento) )
                    IntToStrZero( ARemessa.Count + 1, 6);                            // 395 a 400 - Número sequencial do registro
          ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
        end;
      end;
   end;
end;

function TACBrBancoSicredi.GetLocalPagamento: String;
begin
  Result := Format(ACBrStr(CInstrucaoPagamentoCooperativa), [fpNome]);
end;

procedure TACBrBancoSicredi.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do begin
    wLinha:= '9'                                  + // 001 a 001 - Identificação do registro trailler
             '1'                                  + // 002 a 002 - Identificação do arquivo remessa
             '748'                                + // 003 a 005 - Número do SICREDI
             PadLeft( CodigoCedente, 5, '0')      + // 006 a 010 - Código do cedente
             Space(384)                           + // 011 a 394 - Filler
             IntToStrZero( ARemessa.Count + 1, 6);  // 395 a 400 - Número sequencial do registro
  end;
  ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrBancoSicredi.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, MotivoLinha, I, qtdMotivo: Integer;
  rAgencia, rDigitoAgencia, rConta, rDigitoConta  :String;
  Linha, rCedente, rCNPJCPF, rCodCedente, rEspDoc :String;
  CodMotivo_19,CodMotivo: String;
begin
  fpTamanhoMaximoNossoNum := 20;

  if StrToIntDef(copy(ARetorno[0],77,3),-1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                           'não é um arquivo de retorno do '+ Nome));

  rCNPJCPF      := trim(Copy(ARetorno[0],32,14));
  rCodCedente   := trim(Copy(ARetorno[0],27,5));

  {Informações não são retornadas pelo Sicredi}
  rCedente      := '';
  rAgencia      := '';
  rDigitoAgencia:= '';
  rConta        := '';
  rDigitoConta  := '';

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],111,7),0);

  ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],101,2)+'/'+
                                                          Copy(ARetorno[0],99,2)+'/'+
                                                          Copy(ARetorno[0],95,4),0, 'DD/MM/YYYY' );

  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCodCedente <> OnlyNumber(Cedente.CodigoCedente)) then
       raise Exception.Create(ACBrStr('Agencia\Conta do arquivo inválido'));

    Cedente.Nome         := rCedente;
    Cedente.TipoInscricao:= pJuridica;

    if Copy(rCNPJCPF,1,10) <> '0000000000' then
    begin
      if ValidarCNPJ(rCNPJCPF) <> '' then
      begin
        rCNPJCPF := Copy(rCNPJCPF,4,11);
        Cedente.TipoInscricao := pFisica;
      end;
      Cedente.CNPJCPF := rCNPJCPF;
    end;

    Cedente.CodigoCedente:= rCodCedente;
    Cedente.Agencia      := rAgencia;
    Cedente.AgenciaDigito:= rDigitoAgencia;
    Cedente.Conta        := rConta;
    Cedente.ContaDigito  := rDigitoConta;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  ACBrBanco.TamanhoMaximoNossoNum := 9;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha] ;

    if (Copy(Linha,1,1) <> '1') and (Copy(Linha,1,1) <> '8') then
      Continue;
    if (Copy(Linha,1,1) = '1') then
    begin
      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
        Carteira             := Copy(Linha,14,1);
        if (Carteira = '1') or (Carteira = 'A') then //Cobrança com Registro
        begin
          NossoNumero          := Copy(Linha,48,15);
          Vencimento     := StringToDateTimeDef(Copy(Linha,147,2)+'/'+
                                                Copy(Linha,149,2)+'/'+
                                                Copy(Linha,151,2),0, 'DD/MM/YY' );

          rEspDoc := trim(Copy(Linha,175,1));
          if (rEspDoc = '') or (rEspDoc = 'K') then
            EspecieDoc := 'OS'
          else if (rEspDoc = 'A') then
            EspecieDoc := 'DMI'
          else if (rEspDoc = 'B') then
            EspecieDoc := 'DR'
          else if (rEspDoc = 'C') then
            EspecieDoc := 'NP'
          else if (rEspDoc = 'D') then
            EspecieDoc := 'NR'
          else if (rEspDoc = 'E') then
            EspecieDoc := 'NS'
          else if (rEspDoc = 'G') then
            EspecieDoc := 'RC'
          else if (rEspDoc = 'H') then
            EspecieDoc := 'LC'
          else if (rEspDoc = 'I') then
            EspecieDoc := 'ND'
          else if (rEspDoc = 'J') then
            EspecieDoc := 'DSI';

          ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;
          ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;
          qtdMotivo:=5;
        end
        else                 //Cobrança sem Registro
        begin
          NossoNumero          := Copy(Linha,48,09);
          qtdMotivo:=1;
        end;
        SeuNumero                   := copy(Linha,117,10);
        NumeroDocumento             := copy(Linha,117,10);  //Se repete pois esse layout não se utiliza de campo específico para Numero Documento

        ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;
        ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
        ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
        ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
        ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
        ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
        if StrToIntDef(Copy(Linha,329,8),0) <> 0 then
          DataCredito:= StringToDateTimeDef( Copy(Linha,335,2)+'/'+
                                            Copy(Linha,333,2)+'/'+
                                            Copy(Linha,329,4),0, 'DD/MM/YYYY' );


        CodOcorrencia := StrToIntDef(copy(Linha,109,2),0);
        OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);
        DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                               Copy(Linha,113,2)+'/'+
                                               Copy(Linha,115,2),0, 'DD/MM/YY' );

        //-|Se a ocorrência for igual a  19 - Confirmação de Receb. de Protesto
        //-|Verifica o motivo na posição 295 - A = Aceite , D = Desprezado
        if (CodOcorrencia = 19) then
        begin
          CodMotivo_19:= Copy(Linha,295,1);
          if(CodMotivo_19 = 'A')then
          begin
            MotivoRejeicaoComando.Add(Copy(Linha,295,1));
            DescricaoMotivoRejeicaoComando.Add('A - Aceito');
          end
          else
          begin
            MotivoRejeicaoComando.Add(Copy(Linha,295,1));
            DescricaoMotivoRejeicaoComando.Add('D - Desprezado');
          end;
        end
        else
        begin
          MotivoLinha := 319;  //Motivos da ocorrência
          for i := 0 to qtdMotivo-1 do
          begin
            CodMotivo := IfThen(Copy(Linha,MotivoLinha,2) = '00',
                                '00',
                                Copy(Linha,MotivoLinha,2));
            //Se for o 1º motivo
            if(i = 0)then
            begin
              MotivoRejeicaoComando.Add(IfThen(Copy(Linha,MotivoLinha,2) = '00',
                                               '00',
                                               Copy(Linha,MotivoLinha,2)));
              DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
            end
            else
            begin
              if (CodMotivo <> '00') And (Trim(CodMotivo) <> '') then     //Após o 1º motivo os 00 significam que não existe mais motivo
              begin
                MotivoRejeicaoComando.Add(IfThen(Copy(Linha,MotivoLinha,2) = '00',
                                                 '00',
                                                 Copy(Linha,MotivoLinha,2)));
                DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
              end;
            end;
            MotivoLinha := MotivoLinha + 2; //Incrementa a coluna dos motivos
          end;
        end;
      end;
    end;

    if (Copy(Linha,1,1) = '8') then
    begin
      Titulo.QrCode.emv  := Copy(Linha,135,256);   //tem que ser lido por primeiro para aceitar a
      Titulo.QrCode.txId := Copy(Linha,21,35);     //leitura dos outros campos
      Titulo.QrCode.url  := Copy(Linha,57,77);
    end;
  end;
  fpTamanhoMaximoNossoNum := 5;
end;

function TACBrBancoSicredi.DefineDataDesconto(const ACBrTitulo: TACBrTitulo;
  AFormat: String): String;
begin
  with ACBrTitulo do
  begin
    if (ValorDesconto > 0) then
    begin
      if (DataDesconto > 0) and (TipoDesconto in [ tdValorFixoAteDataInformada, tdPercentualAteDataInformada]) then
        Result := FormatDateTime(AFormat, DataDesconto)
      else
        Result := PadRight('', Length(AFormat), '0');
    end
    else
      Result := PadRight('', Length(AFormat), '0');

  end;

end;

function TACBrBancoSicredi.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String;
begin
  Result := '';
  case ACBrBanco.ACBrBoleto.LayoutRemessa of
    c240: begin
      case TipoOcorrencia of
        toRetornoRegistroConfirmado,
        toRetornoRegistroRecusado,
        toRetornoInstrucaoRejeitada,
        toRetornoAlteracaoDadosRejeitados:
        begin
          case StrtoIntDef(CodMotivo, -1) of
            -1:
            begin
              if(CodMotivo = 'A1') Then
                Result := 'A1 - Rejeição da alteração do número controle do participante'
              else if(CodMotivo = 'A2') Then
                Result := 'A2 - Rejeição da alteração dos dados do sacado'
              else if(CodMotivo = 'A3') Then
                Result := 'A3 - Rejeição da alteração dos dados do sacador/avalista'
              else if(CodMotivo = 'A4') Then
                Result := 'A4 - Sacado DDA'
              else if(CodMotivo = 'A5') Then
                Result := 'A5 - Registro Rejeitado – Título já Liquidado'
              else if(CodMotivo = 'A6') Then
                Result := 'A6 -  Código do Convenente Inválido ou Encerrado'
              else if(CodMotivo = 'A7') Then
                Result := 'A7 -  Título já se encontra na situação Pretendida'
              else if(CodMotivo = 'A8') Then
                Result := 'A8 -  Valor do Abatimento inválido para cancelamento'
              else if(CodMotivo = 'P1') Then
                Result := 'P1 -  Confirmado COM QrCode'
              else if(CodMotivo = 'P2') Then
                Result := 'P2 -  Confirmado SEM QrCode'
              else if(CodMotivo = 'P3') Then
                Result := 'P3 -  Chave Inválida'
              else if(CodMotivo = 'P6') Then
                Result := 'P6 -  txid em duplicidade/invalido';
            end;
            01: Result := '01 - Código do banco inválido';
            02: Result := '02 - Código do registro detalhe inválido';
            03: Result := '03 - Código do segmento inválido';
            04: Result := '04 - Código de movimento não permitido para carteira';
            05: Result := '05 - Código de movimento inválido';
            06: Result := '06 - Tipo/Número de inscrição do cedente inválidos';
            07: Result := '07 - Agência/Conta/DV inválido';
            08: Result := '08 - Nosso número inválido';
            09: Result := '09 - Nosso número duplicado';
            10: Result := '10 - Carteira inválida';
            11: Result := '11 - Forma de cadastramento do título inválido';
            12: Result := '12 - Tipo de documento inválido';
            13: Result := '13 - Identificação da emissão do bloqueto inválida';
            14: Result := '14 - Identificação da distribuição do bloqueto inválida';
            15: Result := '15 - Características da cobrança incompatíveis';
            16: Result := '16 - Data de vencimento inválida';
            17: Result := '17 - Data de vencimento anterior à data de emissão';
            18: Result := '18 - Vencimento fora do prazo de operação';
            19: Result := '19 - Título a cargo de bancos correspondentes com vencimento inferior a XX dias';
            20: Result := '20 - Valor do título inválido';
            21: Result := '21 - Espécie do título inválida';
            22: Result := '22 - Espécie do título não permitida para a carteira';
            23: Result := '23 - Aceite inválido';
            24: Result := '24 - Data da emissão inválida';
            25: Result := '25 - Data da emissão posterior a data de entrada';
            26: Result := '26 - Código de juros de mora inválido';
            27: Result := '27 - Valor/Taxa de juros de mora inválido';
            28: Result := '28 - Código do desconto inválido';
            29: Result := '29 - Valor do desconto maior ou igual ao valor do título';
            30: Result := '30 - Desconto a conceder não confere';
            31: Result := '31 - Concessão de desconto - já existe desconto anterior';
            32: Result := '32 - Valor do IOF inválido';
            33: Result := '33 - Valor do abatimento inválido';
            34: Result := '34 - Valor do abatimento maior ou igual ao valor do título';
            35: Result := '35 - Valor a conceder não confere';
            36: Result := '36 - Concessão de abatimento - já existe abatimento aAnterior';
            37: Result := '37 - Código para protesto inválido';
            38: Result := '38 - Prazo para protesto/negativação inválido';
            39: Result := '39 - Pedido de protesto não permitido para o título';
            40: Result := '40 - Título com ordem de protesto/pedido de negativação emitido';
            41: Result := '41 - Pedido de cancelamento/sustação para títulos sem instrução de protesto';
            42: Result := '42 - Código para baixa/devolução inválido';
            43: Result := '43 - Prazo para baixa/devolução inválido';
            44: Result := '44 - Código da moeda inválido';
            45: Result := '45 - Nome do sacado não informado';
            46: Result := '46 - Tipo/Número de inscrição do sacado inválidos';
            47: Result := '47 - Endereço do sacado não informado';
            48: Result := '48 - CEP inválido';
            49: Result := '49 - CEP sem praça de cobrança (Não localizado)';
            50: Result := '50 - CEP referente a um banco correspondente';
            51: Result := '51 - CEP incompatível com a Unidade da Federação';
            52: Result := '52 - Unidade da Federação inválida';
            53: Result := '53 - Tipo/Número de inscrição do sacador/avalista inválidos';
            54: Result := '54 - Sacador/Avalista não informado';
            55: Result := '55 - Nosso número no banco correspondente não informado';
            56: Result := '56 - Código do banco correspondente não informado';
            57: Result := '57 - Código da multa inválido';
            58: Result := '58 - Data da multa inválida';
            59: Result := '59 - Valor/Percentual da multa inválido';
            60: Result := '60 - Movimento para título não cadastrado';
            61: Result := '61 - Alteração da agência cobradora/DV inválida';
            62: Result := '62 - Tipo de impressão inválido';
            63: Result := '63 - Entrada para título já cadastrado';
            64: Result := '64 - Número da linha inválido';
            79: Result := '79 - Data juros de mora inválido';
            80: Result := '80 - Data do desconto inválida';
            84: Result := '84 - Número autorização inexistente';
            85: Result := '85 - Título com pagamento vinculado';
            86: Result := '86 - Seu Número inválido';
            87: Result := '87 - e-mail/SMS enviado';
            88: Result := '88 - e-mail Lido';
            89: Result := '89 - e-mail/SMS devolvido - endereço de e-mail ou número do celular incorreto';
            90: Result := '90 - e-mail devolvido - caixa postal cheia ';
            91: Result := '91 - e-mail/número do celular do sacado não informado';
            92: Result := '92 - Sacado optante por Bloqueto Eletrônico - e-mail não enviado';
            93: Result := '93 - Código para emissão de bloqueto não permite envio de e-mail ';
            94: Result := '94 - Código da Carteira inválido para envio e-mail';
            95: Result := '95 - Contrato não permite o envio de e-mail';
            96: Result := '96 - Número de contrato inválido';
            97: Result := '97 - Rejeição da alteração do prazo limite de recebimento (a data deve ser informada no campo 28.3.p)';
            98: Result := '98 - Rejeição de dispensa de prazo limite de recebimento';
            99: Result := '99 - Rejeição da alteração do número do título dado pelo cedente';
          else
            Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
          end;
        end;
        toRetornoDebitoTarifas:
        begin
          case StrtoIntDef(CodMotivo, -1) of
            -1:
            begin
              if(CodMotivo = 'S4') Then
                Result := 'S4 - Tarifa de Inclusão Negativação'
              else if(CodMotivo = 'S5') Then
                Result := 'S5 - Tarifa de Exclusão Negativação'
              else
                Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
            end;
            01: Result := '01 - Tarifa de extrato de posição';
            02: Result := '02 - Tarifa de manutenção de título vencido';
            03: Result := '03 - Tarifa de sustação';
            04: Result := '04 - Tarifa de protesto';
            05: Result := '05 - Tarifa de outras instruções';
            06: Result := '06 - Tarifa de outras ocorrências';
            08: Result := '08 - Custas de protesto';
            09: Result := '09 - Custas de sustação de protesto';
            10: Result := '10 - Custas de cartório distribuidor';
            11: Result := '11 - Custas de edital';
            12: Result := '12 - Tarifa sobre devolução de título vencido';
            13: Result := '13 - Tarifa sobre registro cobrada na baixa/liquidação';
            17: Result := '17 - Tarifa sobre prorrogação de vencimento';
            18: Result := '18 - Tarifa sobre alteração de abatimento/desconto';
            19: Result := '19 - Tarifa sobre arquivo mensal (Em ser)';
            20: Result := '20 - Tarifa sobre emissão de bloqueto pré-emitido pelo banco';
          else
            Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
          end;
        end;
        toRetornoLiquidado,
        toRetornoBaixado,
        toRetornoLiquidadoAposBaixaouNaoRegistro:
        begin
          case StrToIntDef(CodMotivo,0) of
            01: Result := '01 - Por saldo';
            02: Result := '02 - Por conta';
            03: Result := '03 - Liquidação no guichê de caixa em dinheiro';
            04: Result := '04 - Compensação eletrônica';
            05: Result := '05 - Compensação convencional';
            06: Result := '06 - Por meio eletrônico';
            07: Result := '07 - Após feriado local';
            08: Result := '08 - Em cartório';
            30: Result := '30 - Liquidação no guichê de caixa em cheque';
            31: Result := '31 - Liquidação em banco correspondente';
            61: Result := '61 - Liquidação PIX ';
          else
            Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
          end;
          if (TipoOcorrencia = toRetornoBaixado) then begin
            case StrToIntDef(CodMotivo,0) of
              09: Result := '09 - Comandada banco';
              10: Result := '10 - Comandada cliente arquivo';
              11: Result := '11 - Comandada cliente on-line';
              12: Result := '12 - Decurso prazo - cliente';
              13: Result := '13 - Decurso prazo - banco';
              14: Result := '14 - Protestado';
              15: Result := '15 - Título excluído';
            end;
          end;
        end;
      else
        Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
      end;
    end;
    c400: begin 
      case TipoOcorrencia of
        toRetornoRegistroConfirmado: //02
          case StrToIntDef(CodMotivo, -1)  of
            00: Result := '00-Ocorrência aceita, entrada confirmada';
          else

            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoRegistroRecusado: //03
          case AnsiIndexStr(CodMotivo,
                           ['A1', 'A2', 'A3', 'A4', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'C5',
                            'C6', 'D5', 'D7', 'F6', 'H7', 'H9', 'I1', 'I2', 'I3', 'I4', 'I5',
                            'I6', 'I7', 'I8', 'I9', 'J1', 'J2', 'J3', 'J4', 'J5', 'J6', 'J7',
                            'J8', 'J9', 'K1', 'K2', 'K3', 'K4', 'K5', 'K6', 'K7', 'K8', 'K9',
                            'L1', 'L2', 'L3', 'L4', 'C1', 'C2', 'C3', 'C4', 'C7', 'C8', 'C9',
                            'A6', 'L7', 'D9']) of
            0: Result:= 'A1-Praça do sacado não cadastrada';
            1: Result:= 'A2-Tipo de cobrança do título divergente com a praça do sacado';
            2: Result:= 'A3-Agência depositária divergente: atualiza o cadastro de praças da agência cedente';
            3: Result:= 'A4-Beneficiário não cadastrado ou possui CNPJ/CPF inválido (Título emitido contendo Sacado e Emitente iguais)';
            4: Result:= 'B4-Tipo de moeda inválido';
            5: Result:= 'B5-Tipo de desconto/juros inválido';
            6: Result:= 'B6-Mensagem padrão não cadastrada';
            7: Result:= 'B7-Seu número inválido';
            8: Result:= 'B8-Percentual de multa inválido';
            9: Result:= 'B9-Valor ou percentual de juros inválido';
            10: Result:= 'C5-Título rejeitado pela centralizadora';
            11: Result:= 'C6-Título já liquidado';
            12: Result:= 'D5-Quantidade inválida no pedido de bloquetos pré-impressos da cobrança sem registro';
            13: Result:= 'D7-Cidade ou Estado do sacado não informado';
            14: Result:= 'F6-Nosso número/Número da parcela fora de sequência - total de parcelas inválido';
            15: Result:= 'H7-Espécie de documento necessita cedente ou avalista PJ';
            16: Result:= 'H9-Dados do título não conferem com disquete';
            17: Result:= 'I1-Sacado e sacador avalista são a mesma pessoa';
            18: Result:= 'I2-Aguardar um dia útil após o vencimento para protestar';
            19: Result:= 'I3-Data do vencimento rasurada';
            20: Result:= 'I4-Vencimento - extenso não confere com número';
            21: Result:= 'I5-Falta data de vencimento no título';
            22: Result:= 'I6-DM/DMI sem comprovante autenticado ou declaração';
            23: Result:= 'I7-Comprovante ilegível para conferência e microfilmagem';
            24: Result:= 'I8-Nome solicitado não confere com emitente ou sacado';
            25: Result:= 'I9-Confirmar se são 2 emitentes. Se sim, indicar os dados dos 2';
            26: Result:= 'J1-Endereço do sacado igual ao do sacador ou do portador';
            27: Result:= 'J2-Endereço do apresentante incompleto ou não informado';
            28: Result:= 'J3-Rua/número inexistente no endereço';
            29: Result:= 'J4-Falta endossodo favorecido para o apresentante';
            30: Result:= 'J5-Data da emissão rasurada';
            31: Result:= 'J6-Falta assinatura do sacador do título';
            32: Result:= 'J7-Nome do apresentante não informado/incompleto/incorreto';
            33: Result:= 'J8-Erro de preenchimento do título';
            34: Result:= 'J9-Título com direito de regresso vencido';
            35: Result:= 'K1-Título apresentado em duplicidade';
            36: Result:= 'K2-Título ja protestado';
            37: Result:= 'K3-Letra de cambio vencida - falta aceite do sacado';
            38: Result:= 'K4-Falta declaração do saldo assinada no título';
            39: Result:= 'K5-Contrato de cambio - Falta conta gráfica';
            40: Result:= 'K6-Ausência do documento físico';
            41: Result:= 'K7-Sacado falecido';
            42: Result:= 'K8-Sacado apresentou quitação do título';
            43: Result:= 'K9-Título de outra jurisdição territorial';
            44: Result:= 'L1-Título com emissão anterior a concordata do sacado';
            45: Result:= 'L2-Sacado consta na lista de falência';
            46: Result:= 'L3-Apresentante não aceita publicação de edital';
            47: Result:= 'L4-Dados do sacado em branco ou inválido';
            48: Result:= 'C1-Data limite para concessão de desconto inválida';
            49: Result:= 'C2-Aceite do título inválido';
            50: Result:= 'C3-Campo alterado na instrução “31 – alteração de outros dados” inválido';
            51: Result:= 'C4-Título ainda não foi confirmado pela centralizadora';
            52: Result:= 'C7-Título já baixado';
            53: Result:= 'C8-Existe mesma instrução pendente de confirmação para este título';
            54: Result:= 'C9-Instrução prévia de concessão de abatimento não existe ou não confirmada';
            55: Result:= 'A6-Data da instrução/ocorrência inválida';
            56: Result:= 'L7-Não permitido cadastro de boleto com negativação automática e protesto automático simultaneamente';
            57: Result:= 'D9-Registro mensagem para título não cadastrado';
          else
            case StrToIntDef(CodMotivo,0) of
              02: Result:= '02-Código do registro detalhe inválido';
              03: Result:= '03-Código da ocorrência inválido';
              04: Result:= '04-Código da ocorrência não permitida para a carteira';
              05: Result:= '05-Código de ocorrência não numérico';
              08: Result:= '08-Nosso número inválido';
              09: Result:= '09-Nosso número duplicado';
              10: Result:= '10-Carteira inválida';
              16: Result:= '16-Data de vencimento inválida';
              18: Result:= '18-Vencimento fora do prazo de operação';
              20: Result:= '20-Valor do título inválido';
              21: Result:= '21-Espécie do título inválida';
              22: Result:= '22-Espécie não permitida para a carteira';
              24: Result:= '24-Data de emissão inválida';
              38: Result:= '38-Prazo para protesto inválido';
              44: Result:= '44-Agência cedente não prevista';
              45: Result:= '45-Nome do sacado inválido';
              46: Result:= '46-Tipo/número de inscrição do sacado inválidos';
              47: Result:= '47-Endereco do sacado não informado';
              48: Result:= '48-CEP inválido';
              49: Result:= '49-Número de Inscrição do sacador/avalista inválido';
              50: Result:= '50-Sacador/avalista não informado';
              63: Result:= '63-Entrada para título já cadastrado';
            else
              Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;
          end;

        toRetornoLiquidado:   //06
          case AnsiIndexStr(CodMotivo, ['A8',  'C7', 'H5', 'H6', 'H8', 'X1', 'X2', 'X3', 'X4', 'X5',
                                        'X0', 'X6', 'X7', 'X8', 'X9', 'XA', 'XB', 'C6']) of
            0: Result:= 'A8-Recebimento da liquidação fora da rede Sicredi - via compensação eletrônica';
            1: Result:= 'C7-Título já baixado';
            2: Result:= 'H5-Recebimento de liquidação fora da rede Sicredi - VLB Inferior - Via compensação';
            3: Result:= 'H6-Recebimento de liquidação fora da rede Sicredi - VLB Superior - Via compensação';
            4: Result:= 'H8-Recebimento de liquidação fora da rede Sicredi - Contingência Via Compe';
            5: Result:= 'X1-Regularização centralizadora - Rede Sicredi';
            6: Result:= 'X2-Regularização centralizadora - Compensação';
            7: Result:= 'X3-Regularização centralizadora - Banco correspondente';
            8: Result:= 'X4-Regularização centralizadora - VLB Inferior - via Compensação';
            9: Result:= 'X5-Regularização centralizadora - VLB Superior - via Compensação';
            10: Result:= 'X0-Pago com cheque';
            11: Result:= 'X6-Pago com cheque - bloqueado 24 horas';
            12: Result:= 'X7-Pago com cheque - bloqueado 48 horas';
            13: Result:= 'X8-Pago com cheque - bloqueado 72 horas';
            14: Result:= 'X9-Pago com cheque - bloqueado 96 horas';
            15: Result:= 'XA-Pago com cheque - bloqueado 120 horas';
            16: Result:= 'XB-Pago com cheque - bloqueado 144 horas';
            17: Result:= 'C6-Título já Liquidado';
          else
            case StrToIntDef(CodMotivo,-1) of
               00: Result:= '00-Ocorrência aceita, liquidação normal';
            else
               Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;
          end;
        toRetornoIntensaoPagamento: //07
          case AnsiIndexStr(CodMotivo,['H5']) of
              0: Result:= 'H5-Recebimento de liquidação fora da rede Sicredi - VLB Inferior - Via compensação';
            else
                Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;
        toRetornoBaixadoViaArquivo: //09
          case StrToIntDef(CodMotivo,-1) of
            00: Result:= '00-Ocorrência aceita, baixado automaticamente via arquivo';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoBaixadoInstAgencia: //10
          case StrToIntDef(CodMotivo,-1) of
            00: Result:= '00-Ocorrência aceita, baixado cfe. instruções na agência';
            14: Result:= '14-Titulo protestado';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoAbatimentoConcedido: //12
          case StrToIntDef(CodMotivo,-1) of
            00: Result:= '00-Ocorrência aceita, abatimento concedido';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoAbatimentoCancelado: //13
          case StrToIntDef(CodMotivo,-1) of
            00: Result:= '00-Ocorrência aceita, abatimento cancelado';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoVencimentoAlterado: //14
          case StrToIntDef(CodMotivo,-1) of
            00: Result:= '00-Ocorrência aceita, vencimento alterado';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoLiquidadoEmCartorio: //15
          case StrToIntDef(CodMotivo,-1) of
            00: Result:= '00-Ocorrência aceita, liquidação em cartório';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoLiquidadoAposBaixaouNaoRegistro: //17
          case AnsiIndexStr(CodMotivo,['A8', 'C6', 'H5', 'H6', 'H8', 'C7']) of
            0: Result:= 'A8-Recebimento da liquidação fora da rede Sicredi - via compensação eletrônica';
            1: Result:= 'C6-Título já liquidado';
            2: Result:= 'H5-Recebimento de liquidação fora da rede Sicredi - VLB Inferior - via compensação';
            3: Result:= 'H6-Recebimento de liquidação fora da rede Sicredi - VLB Superior - via compensação';
            4: Result:= 'H8-Recebimento de liquidação fora da rede Sicredi - Contingência via compe';
            5: Result:= 'C7-Título já baixado';
          else
            case StrToIntDef(CodMotivo,-1) of
              00: Result:= '00-Ocorrência aceita, liquidação após baixa';
            else
               Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;
          end;

        toRetornoRecebimentoInstrucaoProtestar: //19
          case AnsiIndexStr(CodMotivo,['A', 'D']) of
            0: Result:= 'A-Aceito';
            1: Result:= 'D-Desprezado';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoRecebimentoInstrucaoSustarProtesto: //20
            case StrToIntDef(CodMotivo,-1) of
              00: Result:= '00-Ocorrência aceita, sustação de protesto';
            else
               Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;

        toRetornoEntradaEmCartorio: //23
          case AnsiIndexStr(CodMotivo,['G2', 'G3', 'G4', 'G6', 'G7']) of
            0: Result:= 'G2-Título aceito: sem a assinatura do sacado';
            1: Result:= 'G3-Título aceito: rasurado ou rasgado';
            2: Result:= 'G4-Título aceito: falta título(agência cedente deverá enviá-lo)';
            3: Result:= 'G6-Título aceito: sem endosso ou cedente irregular';
            4: Result:= 'G7-Título aceito: valor por extenso diferente do valor numérico';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoEntradaRejeitaCEPIrregular: //24
          case StrToIntDef(CodMotivo,-1) of
            48: Result:= '48-CEP irregular';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoBaixaRejeitada: //27
          case AnsiIndexStr(CodMotivo,['A1', 'C6', 'C5', 'C7']) of
            0: Result:= 'A1-Praça do sacado não cadastrada';
            1: Result:= 'C6-Título já liquidado';
            2: Result:= 'C5-Título rejeitado pela centralizadora';
            3: Result:= 'C7-Título já baixado';
          else
            case StrToIntDef(CodMotivo,-1) of
              00: Result:= '00-Ocorrência aceita, baixa rejeitada';
              07: Result:= '07-Agência\Conta\dígito inválidos';
              08: Result:= '08-Nosso número inválido';
              10: Result:= '10-Carteira inválida';
              15: Result:= '15-Agência\Carteira\Conta\nosso número inválidos';
              40: Result:= '40-Título com ordem de protesto emitida';
              60: Result:= '60-Movimento para título não cadastrado';
            else
               Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;
          end;

        toRetornoDebitoTarifas: //28
          case AnsiIndexStr(CodMotivo,['A9', 'B1', 'B2', 'B3', 'E1', 'F5', 'S4', 'S5']) of
            0: Result:= 'A9-Tarifa de manutenção de título vencido';
            1: Result:= 'B1-Tarifa de baixa da carteira';
            2: Result:= 'B2-Não implementado';
            3: Result:= 'B3-Tarifa de registro de entrada do título';
            4: Result:= 'E1-Não implementado';
            5: Result:= 'F5-Tarifa de entrada na rede SICREDI';
            6: Result:= 'S4-Tarifa de inclusão negativação';
            7: Result:= 'S5-Tarifa de exclusão negativação';
          else
            case StrToIntDef(CodMotivo,0) of
              03 : Result:= '03-Tarifa de sustação';
              04 : Result:= '04-Tarifa de protesto';
              08 : Result:= '08-Tarifa de custas de protesto';
            else
              Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;
          end;

        toRetornoOcorrenciasdoSacado: //29
          case AnsiIndexStr(CodMotivo,['M2']) of
            0 : Result:= 'M2-Não reconhecimento da dívida pelo sacado';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;

        toRetornoAlteracaoDadosRejeitados: //30
          case AnsiIndexStr(CodMotivo,['C5','C6','C7']) of
            0 : Result:= 'C5-Título rejeitado pela centralizadora';
            1 : Result:= 'C6-Título já liquidado';
            2 : Result:= 'C7-Título já baixado';
          else
            case StrToIntDef(CodMotivo,0) of
              01: Result:= '01-Código do Banco inválido';
              05: Result:= '05-Código da ocorrência não numérico';
              08: Result:= '08-Nosso número inválido';
              15: Result:= '15-Agência\Carteira\Conta\nosso número inválidos';
              28: Result:= '28-Não implementado';
              29: Result:= '29-Valor do desconto maior/igual ao valor do título';
              33: Result:= '33-Valor do abatimento inválido';
              34: Result:= '34-Valor do abatimento maior/igual ao valor do título';
              38: Result:= '38-Prazo para protesto inválido';
              39: Result:= '39-Pedido de protesto não permitido para o título';
              40: Result:= '40-Título com ordem de protesto emitido';
              60: Result:= '60-Movimento para título não cadastrado';
            else
              Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
            end;
          end;

        toRetornoInstrucaoRejeitada: //32
          case AnsiIndexStr(CodMotivo,
                            ['A1', 'A2', 'A4', 'A5', 'A6', 'B4', 'B5', 'B6', 'B7',
                             'B8', 'B9', 'C4', 'C5', 'C6', 'C7', 'D2', 'F3', 'F7', 'F8',
                             'F9', 'G1', 'G5', 'G8', 'G9', 'H1', 'L3', 'L4', 'J8',
                             'I9', 'K9', 'A3', 'C8', 'C9', 'J3', 'D1', 'K1', 'G4', 'L6']) of
            0 : Result:= 'A1-Praça do sacado não cadastrada';
            1 : Result:= 'A2-Tipo de cobrança do título divergente com a praça do sacado';
            2 : Result:= 'A4-Cedente não cadastrado ou possui CNPJ/CPF inválido';
            3 : Result:= 'A5-Sacado não cadastrado';
            4 : Result:= 'A6-Data da instrução/ocorrência inválida';
            5 : Result:= 'B4-Tipo de moeda inválido';
            6 : Result:= 'B5-Tipo de desconto/juros inválido';
            7 : Result:= 'B6-Mensagem padrão não cadastrada';
            8 : Result:= 'B7-Seu número inválido';
            9 : Result:= 'B8-Percentual de multa inválido';
            10 : Result:= 'B9-Valor ou percentual de juros inválido';
            11 : Result:= 'C4-Título ainda não foi confirmado pela centralizadora';
            12 : Result:= 'C5-Título rejeitado pela centralizadora';
            13 : Result:= 'C6-Título já liquidado';
            14 : Result:= 'C7-Título já baixado';
            15 : Result:= 'D2-Espécie de documento não permite protesto/negativação de título';
            16 : Result:= 'F3-Instrução inválida, este título está caucionado/descontado';
            17 : Result:= 'F7-Falta de comprovante de prestação de serviço';
            18 : Result:= 'F8-Nome do cedente incompleto/incorreto';
            19 : Result:= 'F9-CNPJ/CPF incompatível com o nome do sacado/sacador avalista';
            20 : Result:= 'G1-CNPJ/CPF do sacador incompatível com a espécie';
            21 : Result:= 'G5-Praça de pagamento incompatível com o endereço';
            22 : Result:= 'G8-Linha digitável maior que o valor do título';
            23 : Result:= 'G9-Tipo de endosso inválido';
            24 : Result:= 'H1-Nome do sacador incompleto/incorreto';
            25 : Result:= 'L3-Apresentante não aceita publicação de edital';
            26 : Result:= 'L4-Dados do sacado em branco ou inválido';
            27 : Result:= 'J8-Erro de preenchimento do título';
            28 : Result:= 'I9-Não previsto no manual';
            29 : Result:= 'K9-Título de outra jurisdição territorial';
            30 : Result:= 'A3-Cooperativa/agência depositária divergente: atualiza o cadastro de praças da Coop./agência beneficiária';
            31 : Result:= 'C8-Existe mesma instrução pendente de confirmação para este título';
            32 : Result:= 'C9-Instrução prévia de concessão de abatimento não existe ou não confirmada';
            33 : Result:= 'J3-Rua/Número inexistente no endereço';
            34 : Result:= 'D1-Título dentro do prazo de vencimento (em dia)';
            35 : Result:= 'K1-Título apresentado em duplicidade';
            36 : Result:= 'G4-Título aceito: Falta título (cooperativa/ag. beneficiária deverá enviá-lo)';
            37 : Result:= 'L6-Tipo de comando de instrução inválida para beneficiário pessoa física.';
          else
            try
              case StrToIntDef(CodMotivo,0) of
                01: Result:= '01-Código do Banco inválido';
                02: Result:= '02-Código do registro detalhe inválido';
                03: Result:= '03-Código da ocorrência inválido';
                04: Result:= '04-Código de ocorrência não permitida para a carteira';
                05: Result:= '05-Código de ocorrência não numérico';
                07: Result:= '07-Agência\Conta\dígito inválidos';
                08: Result:= '08-Nosso número inválido';
                10: Result:= '10-Carteira inválida';
                15: Result:= '15-Agência\Carteira\Conta\nosso número inválidos';
                16: Result:= '16-Data de vencimento inválida';
                17: Result:= '17-Data de vencimento anterior à data de emissão';
                21: Result:= '21-Espécie do título inválida';
                22: Result:= '22-Espécie não permitida para a carteira';
                24: Result:= '24-Data de emissão inválida';
                29: Result:= '29-Valor do desconto maior/igual ao valor do título';
                31: Result:= '31-Concessão de desconto - existe desconto anterior';
                33: Result:= '33-Valor do abatimento inválido';
                34: Result:= '34-Valor do abatimento maior/igual ao valor do título';
                36: Result:= '36-Concessão de abatimento - existe abatimento anterior';
                38: Result:= '38-Prazo para protesto inválido';
                39: Result:= '39-Pedido de protesto não permitido para o título';
                40: Result:= '40-Título com ordem de protesto emitido';
                41: Result:= '41-Pedido cancelamento/sustação sem instrução de protesto';
                45: Result:= '45-Nome do sacado inválido';
                46: Result:= '46-Tipo/número de inscrição do sacado inválidos';
                47: Result:= '47-Endereço do sacado não informado';
                60: Result:= '60-Movimento para título não cadastrado';
              else
                Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
              end;

            except
              Result:= PadLeft(CodMotivo,2,'0') +' - Motivos não identificados';
            end;
          end;

        toRetornoAlteracaoDadosNovaEntrada: //33
          case AnsiIndexStr(CodMotivo,['H4']) of
            0 : Result:= 'H4-Alteração de Carteira';
          else
            Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
          end;


        toRetornoEntradaNegativacaoRejeitada,
        toRetornoExclusaoNegativacaoRejeitada: //81 e 83
           if CodMotivo = 'S1' then
              Result:= 'S1 – Rejeitado pela empresa de negativação parceira.'
           else
              Result:= PadLeft(CodMotivo,2,'0') +' - Motivos não identificados';

        toRetornoExcusaoNegativacaoOutrosMotivos://84;
           case AnsiIndexStr(CodMotivo, ['N1', 'N2', 'N3','N4','N5']) of
             0 : Result:= 'N1-Decurso de Prazo';
             1 : Result:= 'N2-Determinação Judicial';
             2 : Result:= 'N3-Solicitação de Empresa Conveniada';
             3 : Result:= 'N4-Devolução de Comunicado pelos Correios';
             4 : Result:= 'N5-Diversos';
           else
             Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
           end;
        toRetornoOcorrenciaInfOutrosMotivos: //85'
          case AnsiIndexStr(CodMotivo, ['N4','N5']) of
             0 : Result:= 'N4-Devolução de Comunicado pelos Correios';
             1 : Result:= 'N5-Diversos';
           else
             Result:= PadLeft(CodMotivo,2,'0') +' - Outros Motivos';
           end;

      else
        Result:= PadLeft(CodMotivo,2,'0') +' - Motivos não identificados';
      end; //---- Fim Anderson
    end;
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoSicredi.CalcularNomeArquivoRemessa: String;
var
  Sequencia, wMes :Integer;
  NomeFixo, NomeArq: String;
  codMesSicredi : String;
begin

   with ACBrBanco.ACBrBoleto do
   begin
      if NomeArqRemessa <> '' then
         Result := DirArqRemessa + PathDelim + NomeArqRemessa
      else
       begin
         wMes:= StrToInt(FormatDateTime('mm',Now));

         case wMes of
           1..9 : codMesSicredi := Copy(IntToStrZero(wMes,1),1,1);
           10 : codMesSicredi := 'O';
           11 : codMesSicredi := 'N';
           12 : codMesSicredi := 'D';
         end;

         NomeFixo := DirArqRemessa + PathDelim +
                     PadLeft(Copy(Cedente.CodigoCedente, 1, 5), 5, '0')  + codMesSicredi +
                     FormatDateTime( 'dd', Now );

         NomeArq := NomeFixo + '.crm';

         Sequencia := 1;
         while FilesExists(NomeArq) do
         begin
           Inc(Sequencia);

           if Sequencia > 9 then
           begin
             if Sequencia = 10 then
               NomeArq := NomeFixo +'.rm0'
             else
               raise Exception.Create(ACBrStr('Número máximo de 10 arquivos '+
                                              'de remessa alcançado'));
           end
           else
             NomeArq := NomeFixo +'.rm'+IntToStr(Sequencia);

         end;

         Result:= NomeArq;
       end;
   end;
end;

function TACBrBancoSicredi.TipoDescontoToString(const AValue: TACBrTipoDesconto
  ): String;
begin
  // Gera o tipo de desconto numero, utilizado no padrao febraban cnab240
  Result := inherited TipoDescontoToString(AValue);

  // este código de desconto A - Valor - B - Percentual, somente é utilizado no cnab 400
  if self.ACBrBanco.ACBrBoleto.LayoutRemessa = c400 then
    begin
      case AValue of
        tdValorFixoAteDataInformada : Result := 'A';
        tdPercentualAteDataInformada : Result := 'B';
        tdValorAntecipacaoDiaCorrido : Result := 'A';
        tdValorAntecipacaoDiaUtil : Result := 'A';
        tdPercentualSobreValorNominalDiaCorrido : Result := 'B';
        tdPercentualSobreValorNominalDiaUtil : Result := 'B';
      else
        Result := 'A';
      end;
    end;
end;

function TACBrBancoSicredi.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      07: Result := '07-Confirmação do Recebimento da Instrução de desconto';
      08: Result := '08-Confirmação do Recebimento do Cancelamento do desconto';
      24: Result := '24-Retirada de Cartório e Manutenção em Carteira';
      25: Result := '25-Protestado e Baixado';
      26: Result := '26-Instrução Rejeitada';
      27: Result := '27-Confirmação do Pedido de Alteração de Outros Dados';
      36: Result := '36-Baixa Rejeitada';
      51: Result := '51-Título Dda Reconhecido Pelo Pagador';
      52: Result := '52-Título Dda não Reconhecido Pelo Pagador';
      91: Result := '07-Intenção de pagamento';
    end;
  end
  else
  begin
    case CodOcorrencia of
      07: Result := '07-Intenção de pagamento';
      10: Result := '10-Baixado Conforme Instruções da Cooperativa de Crédito';
      15: Result := '15-Liquidação em Cartório';
      24: Result := '24-Entrada Rejeitada Por Cep Irregular';
      27: Result := '27-Baixa Rejeitada';
      29: Result := '29-Rejeição do Pagador';
      32: Result := '32-Instrução Rejeitada';
      33: Result := '33-Confirmação de Pedido de Alteração de Outros Dados';
      34: Result := '34-Retirado de Cartório e Manutenção em Carteira';
      35: Result := '35-Aceite do Pagador';
    end;
  end;

  if (Result <> '') then
  begin
    Result := ACBrSTr(Result);
    Exit;
  end;

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação';
    09: Result := '09-Baixa';
    12: Result := '12-Confirmação do Recebimento Instrução de Abatimento';
    13: Result := '13-Confirmação do Recebimento Instrução de Cancelamento Abatimento';
    14: Result := '14-Confirmação do Recebimento Instrução Alteração de Vencimento';
    17: Result := '17-Liquidação após Baixa ou Liquidação Título não Registrado';
    19: Result := '19-Confirmação do Recebimento Instrução de Protesto';
    20: Result := '20-Confirmação do Recebimento Instrução de Sustação/Cancelamento de Protesto';
    23: Result := '23-Entrada de Título em Cartório';
    28: Result := '28-Débito de Tarifas Custas';
    30: Result := '30-Alteração Rejeitada';
    78: Result := '78-Confirmação de Recebimento de Pedido de Negativação';
    79: Result := '79-Confirmação de Recebimento de Pedido Exclusão de Negativação';
    80: Result := '80-Confirmação de Entrada de Negativação';
    81: Result := '81-Entrada de Negativação Rejeitada';
    82: Result := '82-Confirmação de Exclusão de Negativação';
    83: Result := '83-Exclusão de Negativação Rejeitada';
    84: Result := '84-Exclusão de Negativação por Outros Motivos';
    85: Result := '85-Ocorrência Informacional por Outros Motivos';
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoSicredi.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      08: Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoAlteracaoUsoCedente;
      36: Result := toRetornoBaixaRejeitada;
      51: Result := toRetornoTituloDDAReconhecidoPagador;
      52: Result := toRetornoTituloDDANaoReconhecidoPagador;
      91: Result := toRetornoIntensaoPagamento;
    end;
  end
  else
  begin
    case CodOcorrencia of
      07: Result := toRetornoIntensaoPagamento;
      10: Result := toRetornoBaixadoInstAgencia;
      15: Result := toRetornoLiquidadoEmCartorio;
      24: Result := toRetornoEntradaRejeitaCEPIrregular;
      27: Result := toRetornoBaixaRejeitada;
      29: Result := toRetornoRejeicaoSacado;
      32: Result := toRetornoInstrucaoRejeitada;
      33: Result := toRetornoAlteracaoDadosNovaEntrada;
      34: Result := toRetornoRetiradoDeCartorio;
      35: Result := toRetornoAceiteSacado;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixadoViaArquivo;
    10: Result := toRetornoBaixadoInstAgencia;
    12: Result := toRetornoRecebimentoInstrucaoConcederAbatimento;
    13: Result := toRetornoRecebimentoInstrucaoCancelarAbatimento;
    14: Result := toRetornoRecebimentoInstrucaoAlterarVencimento;
    17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    23: Result := toRetornoEntradaEmCartorio;
    28: Result := toRetornoDebitoTarifas;
    30: Result := toRetornoAlteracaoDadosRejeitados;
    78: Result := toRetornoConfRecPedidoNegativacao;
    79: Result := toRetornoConfRecPedidoExclusaoNegativacao;
    80: Result := toRetornoConfEntradaNegativacao;
    81: Result := toRetornoEntradaNegativacaoRejeitada;
    82: Result := toRetornoConfExclusaoNegativacao;
    83: Result := toRetornoExclusaoNegativacaoRejeitada;
    84: Result := toRetornoExcusaoNegativacaoOutrosMotivos;
    85: Result := toRetornoOcorrenciaInfOutrosMotivos;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoSicredi.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    19 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar protesto e manter na carteira}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoSicredi.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoRecebimentoInstrucaoConcederDesconto            : Result := '07';
      toRetornoRecebimentoInstrucaoCancelarDesconto            : Result := '08';
      toRetornoRetiradoDeCartorio                              : Result := '24';
      toRetornoBaixaPorProtesto                                : Result := '25';
      toRetornoInstrucaoRejeitada                              : Result := '26';
      toRetornoAlteracaoUsoCedente                             : Result := '27';
      toRetornoBaixaRejeitada                                  : Result := '36';
      toRetornoTituloDDAReconhecidoPagador                     : Result := '51';
      toRetornoTituloDDANaoReconhecidoPagador                  : Result := '52';
      toRetornoIntensaoPagamento                               : Result := '91';
    end;
  end
  else
  begin
    case TipoOcorrencia of
      toRetornoIntensaoPagamento                               : Result := '07';
      toRetornoBaixadoInstAgencia                              : Result := '10';
      toRetornoLiquidadoEmCartorio                             : Result := '15';
      toRetornoEntradaRejeitaCEPIrregular                      : Result := '24';
      toRetornoBaixaRejeitada                                  : Result := '27';
      toRetornoRejeicaoSacado                                  : Result := '29';
      toRetornoInstrucaoRejeitada                              : Result := '32';
      toRetornoAlteracaoDadosNovaEntrada                       : Result := '33';
      toRetornoRetiradoDeCartorio                              : Result := '34';
      toRetornoAceiteSacado                                    : Result := '35';
    end;
  end;

  if (Result <> '') then
    Exit;

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                                : Result := '02';
    toRetornoRegistroRecusado                                  : Result := '03';
    toRetornoLiquidado                                         : Result := '06';
    toRetornoBaixado                                           : Result := '09';
    toRetornoBaixadoViaArquivo                                 : Result := '09';

    toRetornoRecebimentoInstrucaoConcederAbatimento            : Result := '12';
    toRetornoRecebimentoInstrucaoCancelarAbatimento            : Result := '13';
    toRetornoRecebimentoInstrucaoAlterarVencimento             : Result := '14';
    toRetornoLiquidadoAposBaixaOuNaoRegistro                   : Result := '17';
    toRetornoRecebimentoInstrucaoProtestar                     : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto                : Result := '20';
    toRetornoEntradaEmCartorio                                 : Result := '23';
    toRetornoDebitoTarifas                                     : Result := '28';
    toRetornoAlteracaoDadosRejeitados                          : Result := '30';
    toRetornoConfRecPedidoNegativacao                          : Result := '78';
    toRetornoConfRecPedidoExclusaoNegativacao                  : Result := '79';
    toRetornoConfEntradaNegativacao                            : Result := '80';
    toRetornoEntradaNegativacaoRejeitada                       : Result := '81';
    toRetornoConfExclusaoNegativacao                           : Result := '82';
    toRetornoExclusaoNegativacaoRejeitada                      : Result := '83';
    toRetornoExcusaoNegativacaoOutrosMotivos                   : Result := '84';
    toRetornoOcorrenciaInfOutrosMotivos                        : Result := '85';
  else
    Result := '02';
  end;
end;

function TACBrBancoSicredi.GerarRegistroHeader240(
  NumeroRemessa: Integer): String;
var TipoInsc: String;
begin
  case ACBrBanco.ACBrBoleto.Cedente.TipoInscricao of
    pFisica:   TipoInsc := '1';
    pJuridica: TipoInsc := '2';
  else
    TipoInsc := '9';
  end;

  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    { HEADER DE ARQUIVO }
    Result := '748'                                                         + // 001 a 003 - Código do banco na compensação "748" SCIREDI
              '0000'                                                        + // 004 a 007 - Lote de serviço "0000"
              '0'                                                           + // 008 a 008 - Tipo de registro = "0" HEADER ARQUIVO
              Space(9)                                                      + // 009 a 017 - Uso exclusivo FEBRABAN/CNAB
              TipoInsc                                                      + // 018 a 018 - Tipo de inscrição da empresa = "1" Pessoa Física "2" Pessoa Jurídica
              PadLeft(OnlyNumber(CNPJCPF), 14, '0')                         + // 019 a 032 - Número de inscrição da empresa
              Space(20)                                                     + // 033 a 052 - Código do convênio no banco (O SICREDI não valida este campo; cfe Manual Agosto 2010 pág. 35)
              PadLeft(OnlyNumber(Agencia), 5, '0')                          + // 053 a 057 - Agência mantenedora da conta
              Space(1)                                                      + // 058 a 058 - Dígito verificador da agência
              PadLeft(OnlyNumber(Conta), 12, '0')                           + // 059 a 070 - Número da Conta
              PadRight(ContaDigito, 1, '0')                                 + // 071 a 071 - DV Conta
              PadRight(DigitoVerificadorAgenciaConta, 1, ' ')               + // 072 a 072 - Dígito verificador da ag / conta
              PadRight(Nome, 30)                                            + // 073 a 102 - Nome da empresa
              PadRight('SICREDI', 30)                                       + // 103 a 132 - Nome do banco = "SICREDI"
              Space(10)                                                     + // 133 a 142 - Uso exclusivo FEBRABAN/CNAB
              '1'                                                           + // 143 a 143 - Código Remessa/Retorno = "1" Remessa "2" Retorno
              FormatDateTime('ddmmyyyy', Now)                               + // 144 a 151 - Data de geração do arquivo
              FormatDateTime('hhnnss', Now)                                 + // 152 a 157 - Hora de geração do arquivo
              IntToStrZero(NumeroRemessa, 6)                                + // 158 a 163 - Número sequencial do arquivo
              PadLeft(IntToStr(fpLayoutVersaoArquivo) , 3, '0')             + // 164 a 166 - Nº da versão do leiaute do arquivo = "081"
              '01600'                                                       + // 167 a 171 - Densidade de gravação do arquivo = "01600"
              Space(20)                                                     + // 172 a 191 - Para uso reservado do banco
              Space(20)                                                     + // 192 a 211 - Para uso reservado da empresa
              Space(29);                                                      // 212 a 240 - Uso exclusivo FEBRABAN/CNAB

    { HEADER DE LOTE }
    Result := Result + #13#10                                               +
              '748'                                                         + // 001 a 003 - Código do banco na compensação "748" SICREDI
              '0001'                                                        + // 004 a 007 - Lote de serviço "0001"
              '1'                                                           + // 008 a 008 - Tipo de registro = "1" HEADER LOTE
              'R'                                                           + // 009 a 009 - Tipo de operação = "R" Arquivo de Remessa
              '01'                                                          + // 010 a 011 - Tipo de serviço = "01" Cobrança
              Space(2)                                                      + // 012 a 013 - Uso exclusivo FEBRABAN/CNAB
              PadLeft(IntToStr(fpLayoutVersaoLote), 3, '0')                 + // 014 a 016 - Nº da versão do leiaute do lote = "040"
              Space(1)                                                      + // 017 a 017 - Uso exclusivo FEBRABAN/CNAB
              TipoInsc                                                      + // 018 a 018 - Tipo de inscrição da empresa = "1" Pessoa Física "2" Pessoa Jurídica
              PadLeft(OnlyNumber(CNPJCPF), 15, '0')                         + // 019 a 033 - Número de inscrição da empresa
              Space(20)                                                     + // 034 a 053 - Código do convênio no banco (O SICREDI não valida este campo; cfe Manual Agosto 2010 pág. 35)
              PadLeft(OnlyNumber(Agencia), 5, '0')                          + // 054 a 058 - Agência mantenedora da conta
              Space(1)                                                      + // 059 a 059 - Dígito verificador da agência
              PadLeft(OnlyNumber(Conta), 12, '0')                           + // 060 a 071 - Número da Canta
              PadRight(ContaDigito,1)                                       + // 072 a 072 - Zeros
              PadRight(DigitoVerificadorAgenciaConta, 1, ' ')               + // 073 a 073 - Dígito verificador da coop/ag/conta
              PadRight(Nome, 30)                                            + // 074 a 103 - Nome da empresa
              Space(40)                                                     + // 104 a 143 - Mensagem 1
              Space(40)                                                     + // 144 a 183 - Mensagem 2
              IntToStrZero(NumeroRemessa, 8)                                + // 184 a 191 - Número remessa/retorno
              FormatDateTime('ddmmyyyy', Now)                               + // 192 a 199 - Data de gravação rem./ret.
              PadRight('', 8, '0')                                          + // 200 a 207 - Data do crédito
              Space(33);                                                      // 208 a 240 - Uso exclusivo FEBRABAN/CNAB

    end;

    Result := UpperCase(Result);
end;

function TACBrBancoSicredi.GerarRegistroTrailler240(
  ARemessa: TStringList): String;
begin
   {REGISTRO TRAILER DO LOTE}
   Result:= IntToStrZero(ACBrBanco.Numero, 3)           + // 001 a 003 - Código do banco na compensação
            '0001'                                      + // 004 a 007 - Lote de serviço = "9999"
            '5'                                         + // 008 a 008 - Tipo do registro = "5" TRAILLER LOTE
            Space(9)                                    + // 009 a 017 - Uso exclusivo FEBRABAN/CNAB
            IntToStrZero(((ARemessa.Count-1) * fpQtdRegsCobranca)+2, 6) + // 018 a 023 - Quantidade de registros no lote
            StringOfChar('0',6)                         + // 024 a 029 - Quantidade de títulos em cobrança
            StringOfChar('0',17)                        + // 030 a 046 - Valor total dos títulos em carteiras
            StringOfChar('0',6)                         + // 047 a 052 - Quantidade de títulos em cobrança
            StringOfChar('0',17)                        + // 053 a 069 - Valor total dos títulos em carteiras
            StringOfChar('0',6)                         + // 070 a 075 - Quantidade de títulos em cobrança
            StringOfChar('0',17)                        + // 076 a 092 - Quantidade de títulos em carteiras
            StringOfChar('0',6)                         + // 093 a 098 - Quantidade de títulos em cobrança
            StringOfChar('0',17)                        + // 099 a 115 - Valor total dos títulos em carteiras
            Space(8)                                    + // 116 a 123 - Número do aviso de lançamento
            Space(117);                                   // 124 a 240 - Uso exclusivo FEBRABAN/CNAB

   {GERAR REGISTRO TRAILER DO ARQUIVO}
   Result:= Result + #13#10 +
            '748'                                       + // 001 a 003 - Código do banco na compensação
            '9999'                                      + // 004 a 007 - Lote de serviço = "9999"
            '9'                                         + // 008 a 008 - Tipo do registro = "9" TRAILLER ARQUIVO
            Space(9)                                    + // 009 a 017 - Uso exclusivo FEBRABAN/CNAB
            '000001'                                    + // 018 a 023 - Quantidade de lotes do arquivo
            IntToStrZero(((ARemessa.Count-1) * fpQtdRegsCobranca)+4, 6) + // 024 a 029 - Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora
            StringOfChar('0', 6)                        + // 030 a 035 - Quantidade de contas para conciliação (lotes)
            Space(205);                                   // 036 a 240 - Uso exclusivo FEBRABAN/CNAB
end;

function TACBrBancoSicredi.GerarRegistroTransacao240(
  ACBrTitulo: TACBrTitulo): String;
var
    AceiteStr, CodProtestoNegativacao, DiasProtestoNegativacao, TipoSacado, ATipoBoleto, ATipoDoc: String;
    Especie, EndSacado, Ocorrencia: String;
    TipoAvalista: Char;
    lDataDesconto: String;
    LCodigoMoraJuros : String;
    LTitulo: TACBrTitulo;
begin
  with ACBrBanco.ACBrBoleto.Cedente, ACBrTitulo do
  begin
    {Aceite}
    case Aceite of
      atSim: AceiteStr := 'A';
      atNao: AceiteStr := 'N';
    end;

    case AnsiIndexStr(EspecieDoc, ['DMI', 'DSI', 'DR', 'LC', 'NP', 'NPR', 'NS', 'RC', 'ND', 'BP']) of
      0 : Especie := '03'; //DMI duplicata mercantil por indicação
      1 : Especie := '05'; //DSI duplicata de serviço por indicação
      2 : Especie := '06'; //DR duplicata rural
      3 : Especie := '07'; //LC letra de câmbio
      4 : Especie := '12'; //NP nota promissóri
      5 : Especie := '13'; //NPR nota promissória rural
      6 : Especie := '16'; //NS nota de seguro
      7 : Especie := '17'; //RC recibo
      8 : Especie := '19'; //ND nota de débito
      9 : Especie := '32'; //Boleto Proposta
    else
      Especie := '99'; //Outros
    end;

    {Pegando Código da Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento             : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento              : Ocorrencia := '06'; {Alteração de vencimento}
      toRemessaConcederDesconto               : Ocorrencia := '07'; {Concessão de desconto}
      toRemessaCancelarDesconto               : Ocorrencia := '08'; {Cancelamento de desconto}
      toRemessaProtestar                      : Ocorrencia := '09'; {Pedido de protesto}
      toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '10'; {Sustar protesto e baixar título}
      toRemessaCancelarInstrucaoProtesto      : Ocorrencia := '11'; {Sustar protesto e manter na carteira}
      toRemessaAlterarJurosMora               : Ocorrencia := '12'; {Alteração de juros de mora }
      toRemessaDispensarJuros                 : Ocorrencia := '13'; {Dispensar cobrança de juros de mora }
      toRemessaAlterarDesconto                : Ocorrencia := '16'; {Alteração do valor de desconto }
      toRemessaNaoConcederDesconto            : Ocorrencia := '17'; {Não conceder desconto }
      toRemessaOutrasOcorrencias              : Ocorrencia := '31'; {Alteração de Outros Dados}
      toRemessaNegativacaoSerasa              : Ocorrencia := '45'; {Negativar Serasa}
      toRemessaExcluirNegativacaoSerasa       : Ocorrencia := '75'; {Excluir Negativação Serasa}
      toRemessaExcluirNegativacaoSerasaBaixar : Ocorrencia := '76'; {Excluir Negativação Serasa e Baixar}

    else
       Ocorrencia := '01';{Entrada de títulos}
    end;

    {Protesto}
    CodProtestoNegativacao := '3'; //Não protestar/negativar
    DiasProtestoNegativacao := '00';
    if (DataProtesto > 0) and (DataProtesto > Vencimento) then
    begin
      CodProtestoNegativacao := '1'; //Protestar dias corridos
      DiasProtestoNegativacao := PadLeft(IntToStr(DaysBetween(DataProtesto, Vencimento)), 2, '0');
    end
    else if (DataNegativacao > 0) and (DataNegativacao >= Vencimento + 3) then // mínimo de 3 dias de negativacaoserasa
    begin
      {Pegando campo Negativação Serasa}
      CodProtestoNegativacao := '8'; //Negativação sem Protesto
      DiasProtestoNegativacao := IntToStrZero(DaysBetween(DataNegativacao,Vencimento),2);
    end;

    //75-Excluir Negativação Serasa  76-Excluir Negativação Serasa e Baixar
    if (Ocorrencia = '75') or (Ocorrencia = '76') then
      CodProtestoNegativacao := '9'; //Cancelamento protesto automático/negativação

    {Sacado}
    case Sacado.Pessoa of
      pFisica:   TipoSacado := '1';
      pJuridica: TipoSacado := '2';
    else
      TipoSacado := '9';
    end;

    if NaoEstaVazio(ACBrBoleto.Cedente.PIX.Chave) then
      fpQtdRegsCobranca := 4
    else
      fpQtdRegsCobranca := 3;

    EndSacado := Sacado.Logradouro;
    if (Sacado.Numero <> '') then
      EndSacado := EndSacado + ', ' + Sacado.Numero;
    EndSacado := PadRight(trim(EndSacado), 40);


    {Avalista}
    case Sacado.SacadoAvalista.Pessoa of
      pFisica:   TipoAvalista := '1';
      pJuridica: TipoAvalista := '2';
    else
      TipoAvalista := '0';
    end;

     {Pegando Tipo de Boleto}
     case ACBrBoleto.Cedente.ResponEmissao of
       tbCliEmite        : ATipoBoleto := '2' + '2';
       tbBancoEmite      : ATipoBoleto := '1' + '1';
       tbBancoReemite    : ATipoBoleto := '4' + '1';
       tbBancoNaoReemite : ATipoBoleto := '5' + '2';
     end;

    {Codigo Mora Juros}
    LCodigoMoraJuros := DefineCodigoMoraJuros(ACBrTitulo);

    {Tipo Documento}
    ATipoDoc:= DefineTipoDocumento;

    {Data Desconto}
    lDataDesconto := DefineDataDesconto(ACBrTitulo);

    {SEGMENTO P}
    Result:= '748'                                                            + // 001 a 003 - Código do banco na compensação
             '0001'                                                           + // 004 a 007 - Lote de serviço = "0001"
             '3'                                                              + // 008 a 008 - Tipo de registro = "3" DETALHE
             IntToStrZero(
               (fpQtdRegsCobranca * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 1 , 5)   + // 009 a 013 - Nº sequencial do registro do lote
             'P'                                                              + // 014 a 014 - Cód. segmento do registro detalhe
             Space(1)                                                         + // 015 a 015 - Uso exclusivo FEBRABAN/CNAB
             Ocorrencia                                                       + // 016 a 017 - Código de movimento remessa
             PadLeft(OnlyNumber(Agencia), 5,'0')                              + // 018 a 022 - Agência mantenedora da conta
             Space(1)                                                         + // 023 a 023 - Dígito verificador da agência
             PadLeft(OnlyNumber(Conta), 12, '0')                              + // 024 a 035 - Número da conta corrente
             ContaDigito	                                                  + // 036 a 036 - Digito da conta 
             Space(1)                                                         + // 037 a 037 - Dígito verificador da coop/ag/conta
             PadRight(OnlyNumber(MontarCampoNossoNumero(ACBrTitulo)), 20, '0')+ // 038 a 057 - Identificação do título no banco
             '1'                                                              + // 058 a 058 - Código da carteira
             '1'                                                              + // 059 a 059 - Forma de cadastro do título no banco
             ATipoDoc                                                         + // 060 a 060 - Tipo de documento
             ATipoBoleto                                                      + // 061 a 062 - Identificação de emissão do bloqueto + 062 a 062 - Identificação da distribuição
             PadRight(NumeroDocumento, 15)                                    + // 063 a 077 - Nº do documento de cobrança
             FormatDateTime('ddmmyyyy', Vencimento)                           + // 078 a 085 - Data de vencimento do título
             IntToStrZero(Round(ValorDocumento * 100), 15)                    + // 086 a 100 - Valor nominal do título
             '00000'                                                          + // 101 a 105 - Coop./Ag. encarregada da cobrança
             Space(1)                                                         + // 106 a 106 - Dígito verificador da coop./agência
             PadLeft(Especie, 2, '0')                                         + // 107 a 108 - Espécie do título
             AceiteStr                                                        + // 109 a 109 - Identificação de título aceito/não aceito
             FormatDateTime('ddmmyyyy', DataDocumento)                        + // 110 a 117 - Data da emissão do título
             LCodigoMoraJuros                                                 + // 118 a 118 - Código do juro de mora
             IfThen((DataMoraJuros > 0),
                     FormatDateTime('ddmmyyyy', DataMoraJuros),
                                    '00000000')                               + // 119 a 126 - Data do juro de mora
             IntToStrZero(Round(ValorMoraJuros * 100), 15)                    + // 127 a 141 - Juros de mora por dia/taxa
             TipoDescontoToString(ACBrTitulo.TipoDesconto)                    + // 142 a 142 - Código do desconto 1
             lDataDesconto                                                    + // 143 a 150 - Data do desconto 1
             IntToStrZero(Round(ValorDesconto * 100), 15)                     + // 151 a 165 - Valor percentual a ser concedido
             IntToStrZero(Round(ValorIOF * 100), 15)                          + // 166 a 180 - Valor do IOF a ser recolhido
             IntToStrZero(Round(ValorAbatimento * 100), 15)                   + // 181 a 195 - Valor do abatimento
             PadRight(SeuNumero, 25)                                          + // 196 a 220 - Identificação do título na empresa
             CodProtestoNegativacao                                           + // 221 a 221 - Código para protesto
             DiasProtestoNegativacao                                          + // 222 a 223 - Número de dias para protesto
             '1'                                                              + // 224 a 224 - Código para baixa/devolução
             '000'                                                            + // 225 a 227 - Nº de dias para baixa/devolução (O Sicredi não utiliza esse campo)
             '09'                                                             + // 228 a 229 - Código da moeda = "09"
             PadRight('', 10, '0')                                            + // 230 a 239 - Nº do contrato da operação de crédito
             Space(1);                                                          // 240 a 240 - Uso exclusivo FEBRABAN/CNAB

    {SEGMENTO Q}
    Result:= Result + #13#10 +
             '748'                                                          + // 001 a 003 - Código do banco na compensação
             '0001'                                                         + // 004 a 007 - Lote de serviço = "0001"
             '3'                                                            + // 008 a 008 - Tipo de registro = "3" DETALHE
             IntToStrZero(
               (fpQtdRegsCobranca * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 2 , 5) + // 009 a 013 - Nº sequencial do registro do lote
             'Q'                                                            + // 014 a 014 - Cód. segmento do registro detalhe
             Space(1)                                                       + // 015 a 015 - Uso exclusivo FEBRABAN/CNAB
             Ocorrencia                                                     + // 016 a 017 - Código de movimento de remessa
             TipoSacado                                                     + // 018 a 018 - Tipo de inscrição
             PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                   + // 019 a 033 - Número de inscrição
             PadRight(TiraAcentos(Sacado.NomeSacado), 40)                   + // 034 a 073 - Nome
             EndSacado                                                      + // 074 a 113 - Endereço
             PadRight(TiraAcentos(Sacado.Bairro), 15)                       + // 114 a 128 - Bairro
             Copy(PadLeft(OnlyNumber(Sacado.CEP),8,'0'),1,5)                + // 129 a 133 - CEP
             Copy(PadLeft(OnlyNumber(Sacado.CEP),8,'0'),6,3)                + // 134 a 136 - Sufixo do CEP
             PadRight(TiraAcentos(Sacado.Cidade), 15)                       + // 137 a 151 - Cidade
             PadLeft(Sacado.UF, 2)                                          + // 152 a 153 - Unidade da Federação
             TipoAvalista                                                   + // 154 a 154 - Tipo de inscrição
             PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 15, '0')    + // 155 a 169 - Número de inscrição
             PadRight(TiraAcentos(Sacado.SacadoAvalista.NomeAvalista),40,' ')            + // 170 a 209 - Nome do sacador/avalista
             PadRight('', 3, '0')                                           + // 210 a 212 - Cód. bco corresp. na compensação
             Space(20)                                                      + // 213 a 232 - Nosso nº no banco correspondente
             Space(8);
                                                                              // 233 a 240 - Uso exclusivo FEBRABAN/CNAB
      {SEGMENTO R - Opcional - Exclusivo para cadastramento de multa ao título}
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                           + // Código do banco
               '0001'                                                      + // Número do lote
               '3'                                                         + // Tipo do registro: Registro detalhe
               IntToStrZero(
               (fpQtdRegsCobranca * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 3 , 5)+ // 9 a 13 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
               'R'                                                         + // Código do segmento do registro detalhe
               ' '                                                         + // Uso exclusivo FEBRABAN/CNAB: Branco
               Ocorrencia                                                  + // 16 a 17 - Código de movimento

               TipoDescontoToString(ACBrTitulo.TipoDesconto2)              + // 18  tipo de desconto 2
               IfThen(ValorDesconto2 = 0, '00000000', FormatDateTime('ddmmyyyy', DataDesconto2)) + // 19 - 26 Data do Desconto 2
               IntToStrZero(Round(ValorDesconto2 * 100), 15)               + // 27 - 41 Valor/Percentual

               TipoDescontoToString(ACBrTitulo.TipoDesconto3)              + // 42 tipo de desconto 3
               IfThen(ValorDesconto3 = 0, '00000000', FormatDateTime('ddmmyyyy', DataDesconto3)) +  // 43-50 data do desconto 3
               IntToStrZero(Round(ValorDesconto3 * 100), 15)               +// 51-65 Valor ou percentual a ser concedido

               '2'                                                         + // 66 Código da multa - 2 valor percentual
               IfThen((DataMulta > 0),
                       FormatDateTime('ddmmyyyy', DataMulta),
                                      '00000000')                          + // 67 - 74 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
               IfThen((PercentualMulta > 0),
                      IntToStrZero(round(PercentualMulta * 100), 15),
                      PadLeft('', 15, '0'))                                + // 75 - 89 Percentual de multa. Informar zeros se não cobrar
               space(10)                                                   + // 90-99 Informações do sacado
  			   space(40)    											   + // 100-139 Menssagem livre
               space(40)    											   + // 140-179 Menssagem livre
               space(20)                                                   + // 180-199 Uso da FEBRABAN "Brancos"
               PadLeft('0', 08, '0')                                       + // 200-207 Código oco. sacado "0000000"
               PadLeft('0', 3, '0')                                        + // 208-210 Código do banco na conta de débito "000"
               PadLeft('0', 5, '0')                                        + // 211-215 Código da ag. debito
               '0'                                                         + // 216 Digito da agencia (O Sicredi não usa esse campo, preencher com zeros)
               PadLeft('0', 12, '0')                                       + // 217-228 Conta corrente para debito
               ' '                                                         + // 229 Digito conta de debito
               ' '                                                         + // 230 Dv agencia e conta
               '0'                                                         + // 231 Aviso debito automatico
               space(9);                                                     // 232-240 Uso FEBRABAN


      {8.7 Registro Híbrido}
      if NaoEstaVazio(ACBrBoleto.Cedente.PIX.Chave) then
      begin
        Result:= Result + #13#10 +
                  IntToStrZero(ACBrBanco.Numero, 3)                              + // Código do banco
                  '0001'                                                         + // Número do lote
                  '3'                                                            + // Tipo do registro: Registro detalhe
                  IntToStrZero(
                  (fpQtdRegsCobranca * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 4 , 5)   + // 009 a 013 - Número seqüencial do registro no lote - Cada registro possui dois segmentos
                  'Y'                                                            + // 014 a 014 - Código para identificar o segmento do registro
                  ' '                                                            + // 015 a 015 - Sem preenchimento
                  '01'                                                           + // 016 a 017 - Domínio ( 01- Entrada de títulos )
                  '04'                                                           + // 018 a 019 - Identificação registro opcional
                  Space(50)                                                      + // 020 a 069 - Sem preenchimento
                  Space(2)                                                       + // 070 a 071 - Sem preenchimento
                  Space(9)                                                       + // 072 a 080 - Sem preenchimento
                  Space(1)                                                       + // 081 a 081 - Tipo de chave Em branco (Sicredi não valida)
                  PadRight(ACBrBoleto.Cedente.PIX.Chave, 77, ' ')                + // 082 a 158 - Chave PIX (chave aleatória disponibilizada no PIX)
                  PadRight(QrCode.txId, 35, ' ')                                 + // 159 a 193 - TXID
                  Space(47)                                                      ; // 194 a 240 - Filler ( Deixar em Branco (sem preenchimento) )
      end;

  end;

  Result := IfThen(NaoEstaVazio(ACBrBanco.ACBrBoleto.Cedente.PIX.Chave),Result,UpperCase(Result));
end;

procedure TACBrBancoSicredi.LerRetorno240(ARetorno: TStringList);
var Titulo: TACBrTitulo;
    SegT, SegU, SegY: String;
    ContLinha, IdxMotivo: Integer;
    rCedente, rCNPJCPF, rCodCedente, rAgencia, rDigitoAgencia: String;
    rConta, rDigitoConta: String;
begin

   if (StrToIntDef(Copy(ARetorno[0], 1, 3), -1) <> Numero) then
     raise Exception.Create(ACBrStr('"'+ ACBrBanco.ACBrBoleto.NomeArqRetorno +
                                       '" não é um arquivo de retorno do(a) '+ UpperCase(Nome)));

   rCedente       := Trim(Copy(ARetorno[0],73,30));

   if ACBrBanco.ACBrBoleto.Cedente.TipoInscricao = pJuridica then
    rCNPJCPF       := Copy(ARetorno[0],19,14)
   else
    rCNPJCPF       := Copy(ARetorno[0],22,11);
    
   rCodCedente    := Copy(ARetorno[0],33,5);
   rAgencia       := Copy(ARetorno[0],54,4);
   rDigitoAgencia := Copy(ARetorno[0],58,1);
   rConta         := Copy(ARetorno[0],59,12);
   rDigitoConta   := Copy(ARetorno[0],71,1);

   with ACBrBanco.ACBrBoleto do
   begin
      NumeroArquivo   := StrToIntDef( Copy(ARetorno[0],158,6),0 );
      DataArquivo     := StringToDateTimeDef( Copy(ARetorno[0],144,2) +'/'+
                                              Copy(ARetorno[0],146,2) +'/'+
                                              Copy(ARetorno[0],148,4),
                                              0, 'DD/MM/YYYY' );
      if (Copy(ARetorno[1], 200, 2) <> '00') and (Trim(Copy(ARetorno[1], 200, 2)) <> '') then
        begin
        DataCreditoLanc := StringToDateTimeDef( Copy(ARetorno[1], 200, 2) +'/'+
                                                Copy(ARetorno[1], 202, 2) +'/'+
                                                Copy(ARetorno[1], 204, 4),
                                                0, 'DD/MM/YYYY' );
        end;

   end;

   ValidarDadosRetorno(rAgencia, rCodCedente, '', True);
   with ACBrBanco.ACBrBoleto do
   begin
      Cedente.Nome := rCedente;

      if Copy(rCNPJCPF,1,10) <> '0000000000' then 
         Cedente.CNPJCPF      := rCNPJCPF;

      Cedente.CodigoCedente:= rCodCedente;
      Cedente.Agencia      := rAgencia;
      Cedente.AgenciaDigito:= rDigitoAgencia;
      Cedente.Conta        := rConta;
      Cedente.ContaDigito  := rDigitoConta;

      Cedente.TipoInscricao:= pJuridica;
      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 9;
   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      SegT := ARetorno[ContLinha] ;
      SegU := ARetorno[ContLinha + 1] ;

      if (SegT[14] <> 'T') then
        Continue;
//      else if (Copy(SegT,16,2) = '28') then // se a ocorrência do campo 016~017 for = 28
//         Continue;

      SegY := ARetorno[ContLinha + 2] ;
      if (SegY[14] <> 'Y') then
        SegY := '';


      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
        if (SegT[133] = '1') then
          Sacado.Pessoa := pFisica
        else if (SegT[133] = '2') then
          Sacado.Pessoa := pJuridica
        else
          Sacado.Pessoa := pOutras;
        case Sacado.Pessoa of
          pFisica:   Sacado.CNPJCPF := Copy(SegT, 138, 11);
          pJuridica: Sacado.CNPJCPF := Copy(SegT, 135, 14);
        else
          Sacado.CNPJCPF := Copy(SegT, 134, 15);
        end;
        Sacado.NomeSacado := Trim(Copy(SegT, 149, 40));

        NumeroDocumento      := Trim(Copy(SegT,59,15));
        SeuNumero            := Trim(Copy(SegT,106,25));
        Carteira             := Copy(SegT,58,1);
        if ACBrBanco.ACBrBoleto.LerNossoNumeroCompleto then
          NossoNumero          := Trim(Copy(SegT,38, TamanhoMaximoNossoNum))
        else
          NossoNumero          := Trim(Copy(SegT,38, 8));

        Vencimento           := StringToDateTimeDef( Copy(SegT,74,2) +'/'+
                                                     Copy(SegT,76,2) +'/'+
                                                     Copy(SegT,78,4),
                                                     0, 'DD/MM/YYYY' );
        ValorDocumento       := StrToFloatDef(Copy(SegT, 82,15),0)/100;
        ValorDespesaCobranca := StrToFloatDef(Copy(SegT,199,15),0)/100;
        ValorMoraJuros       := StrToFloatDef(Copy(SegU, 18,15),0)/100;
        ValorDesconto        := StrToFloatDef(Copy(SegU, 33,15),0)/100;
        ValorAbatimento      := StrToFloatDef(Copy(SegU, 48,15),0)/100;
        ValorIOF             := StrToFloatDef(Copy(SegU, 63,15),0)/100;
        ValorPago            := StrToFloatDef(Copy(SegU, 78,15),0)/100;
        ValorRecebido        := StrToFloatDef(Copy(SegU, 93,15),0)/100;
        ValorOutrasDespesas  := StrToFloatDef(Copy(SegU,108,15),0)/100;
        ValorOutrosCreditos  := StrToFloatDef(Copy(SegU,123,15),0)/100;
        DataOcorrencia       := StringToDateTimeDef( Copy(SegU,138,2) +'/'+
                                                     Copy(SegU,140,2) +'/'+
                                                     Copy(SegU,142,4),
                                                     0, 'DD/MM/YYYY' );
        if Trim(Copy(SegU,146,2))<>'' then
          begin
          DataCredito          := StringToDateTimeDef( Copy(SegU,146,2) +'/'+
                                                      Copy(SegU,148,2) +'/'+
                                                      Copy(SegU,150,4),
                                                      0, 'DD/MM/YYYY' );
          end
        else
          begin
          DataCredito          := DataOcorrencia;
          end;

        OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(Copy(SegT, 16, 2), 0));

        if Trim(Copy(SegY,82,77))<>'' then
          QrCode.PIXQRCodeDinamico(Trim(Copy(SegY,82,77)),Trim(Copy(SegY,159,35)), Titulo);


        IdxMotivo := 214;
        while (IdxMotivo < 223) do
        begin
          if (trim(Copy(SegT, IdxMotivo, 2)) <> '') then begin
            MotivoRejeicaoComando.Add(Copy(SegT, IdxMotivo, 2));
            DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, Copy(SegT, IdxMotivo, 2)));
          end;
          Inc(IdxMotivo, 2);
        end;
      end;
   end;
   ACBrBanco.TamanhoMaximoNossoNum := 5;
end;

function TACBrBancoSicredi.CompOcorrenciaOutrosDadosToCodigo(
  const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String;
begin
  Result := ' ';
  case CompOcorrencia of
  TCompDesconto:                      Result := 'A';
  TCompJurosDia:                      Result := 'B';
  TCompDescontoDiasAntecipacao:       Result := 'C';
  TCompDataLimiteDesconto:            Result := 'D';
  TCompCancelaProtestoAutomatico:     Result := 'E';
  TCompCarteiraCobranca:              Result := 'F';
  TCompCancelaNegativacaoAutomatica:  Result := 'G';
  end;
end;

function TACBrBancoSicredi.CompOcorrenciaOutrosDadosToDescricao(
  const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String;
begin
  Result := '';
  case CompOcorrencia of
  TCompDesconto:                      Result := 'Desconto';
  TCompJurosDia:                      Result := 'Juros por dia';
  TCompDescontoDiasAntecipacao:       Result := 'Desconto por dia de antecipação ';
  TCompDataLimiteDesconto:            Result := 'Data limite para concessão de desconto';
  TCompCancelaProtestoAutomatico:     Result := 'Cancelamento de protesto automático ';
  TCompCarteiraCobranca:              Result := 'Carteira de cobrança';
  TCompCancelaNegativacaoAutomatica:  Result := 'Cancelamento de negativação automática';
  end;

  Result := ACBrSTr(Result);
end;

end.
