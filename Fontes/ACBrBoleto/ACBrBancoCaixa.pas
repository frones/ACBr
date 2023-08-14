{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: João Elson                                      }
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

unit ACBrBancoCaixa;

interface

uses
  Classes, SysUtils, Contnrs, ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrCaixaEconomica}

  TACBrCaixaEconomica = class(TACBrBancoClass)
   protected
    function GetLocalPagamento: String; override;
    function DefineAceiteImpressao(const ACBrTitulo: TACBrTitulo): String; override;
    procedure EhObrigatorioAgenciaDV; override;
   private
    fValorTotalDocs:Double;
    fQtRegLote: Integer;
    function RetornaModalidade(const ACBrTitulo :TACBrTitulo): String;
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
    function RetornaCodCarteira(const Carteira: string; const ACBrTitulo : TACBrTitulo): integer;
    function DefineCodigoCedente(const ACBrCedente :TACBrCedente): String;
    function ConverteModalidadeEmCodCarteira(const Modalidade: Integer): String;
   public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override;
    function CalcularDVCedente(const ACBrTitulo: TACBrTitulo ): String;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    procedure LerRetorno240(ARetorno: TStringList); override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa : TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override; 
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): string; override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodigoLiquidacao_Descricao( CodLiquidacao : Integer) : String;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): string; override;
   end;

implementation

uses StrUtils, Variants,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;

constructor TACBrCaixaEconomica.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 0;
   fpNome                  := 'Caixa Economica Federal';
   fpNumero                := 104;
   fpTamanhoAgencia        := 5;
   fpTamanhoMaximoNossoNum := 15;
   fpTamanhoCarteira       := 2;
   fValorTotalDocs         := 0;
   fQtRegLote              := 0;
   fpLayoutVersaoArquivo := 50;
   fpLayoutVersaoLote    := 30;
   fpCodigosMoraAceitos    := '123';

   fpOrientacoesBanco.Clear;
   fpOrientacoesBanco.Add(ACBrStr(
                          'SAC CAIXA: 0800 726 0101 (informações, reclamações, sugestões e elogios) ' + sLineBreak +
                          'Para pessoas com deficiência auditiva ou de fala: 0800 726 2492 ' + sLineBreak +
                          'Ouvidoria: 0800 725 7474 - caixa.gov.br'));
   
end;

function TACBrCaixaEconomica.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
var
  Num, ACarteira, ANossoNumero, Res :String;
begin
   Result := '0';
   if (ACBrTitulo.Carteira = 'RG') then
      ACarteira := '1'
   else if (ACBrTitulo.Carteira = 'SR')then
      ACarteira := '2'
   else
      raise Exception.Create( ACBrStr('Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"') ) ;

   ANossoNumero := OnlyNumber(ACBrTitulo.NossoNumero);

   if ACBrTitulo.CarteiraEnvio = tceCedente then //O Cedente é quem envia o boleto
      Num := ACarteira + '4' + PadLeft(ANossoNumero, 15, '0')
   else
      Num := ACarteira + '1' + PadLeft(ANossoNumero, 15, '0'); //o Banco é quem Envia

   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal   := 2;
   Modulo.MultiplicadorInicial := 9;
   Modulo.Documento := Num;
   Modulo.Calcular;

   Res:= IntToStr(Modulo.ModuloFinal);

   if Length(Res) > 1 then
      Result := '0'
   else
      Result := Res[1];

end;

function TACBrCaixaEconomica.CalcularDVCedente(const ACBrTitulo: TACBrTitulo): String;
var
  Num, Res, codCedente: string;
begin
    codCedente := DefineCodigoCedente(ACBrTitulo.ACBrBoleto.Cedente);
    Num := RightStr(codCedente,Length(codCedente));
    Modulo.CalculoPadrao;
    Modulo.MultiplicadorFinal   := 2;
    Modulo.MultiplicadorInicial := 9;
    Modulo.Documento := Num;
    Modulo.Calcular;
    Res := intTostr(Modulo.ModuloFinal);

    if Length(Res) > 1 then
       Result := '0'
    else
       Result := Res[1];
end;

function TACBrCaixaEconomica.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamentoLoterica);
end;

function TACBrCaixaEconomica.RetornaModalidade(const ACBrTitulo :TACBrTitulo): String;
var
  AModalidade : String;
begin
   with ACBrTitulo do
   begin
    AModalidade := OnlyNumber(ACBrBoleto.Cedente.Modalidade);

      if (ACBrTitulo.Carteira = 'RG') then        {carterira registrada}
        begin
        if ACBrTitulo.CarteiraEnvio = tceCedente then
          AModalidade := '14'
        else
          AModalidade := '11'
      end
      else if (ACBrTitulo.Carteira = 'SR')then     {carteira 2 sem registro}
      begin
        if ACBrTitulo.CarteiraEnvio = tceCedente then
          AModalidade := '24'
        else
          AModalidade := '21'
      end
      else
         raise Exception.Create( ACBrStr('Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"') ) ;
      end;

    Result := AModalidade;

  end;

function TACBrCaixaEconomica.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
var
  ANossoNumero, AModalidade :String;
begin
   with ACBrTitulo do
   begin
      ANossoNumero := OnlyNumber(NossoNumero);
      AModalidade:= RetornaModalidade(ACBrTitulo);

      if (ACBrTitulo.Carteira = 'RG') or (ACBrTitulo.Carteira = 'SR') then
      begin
//        if ACBrTitulo.CarteiraEnvio = tceCedente then
//          ANossoNumero := AModalidade + PadLeft(ANossoNumero, 15, '0')
//        else if (StrToIntDef(ANossoNumero,0)) <> 0  then
//          ANossoNumero := AModalidade + PadLeft(ANossoNumero, 15, '0')
//        else
          ANossoNumero := AModalidade + PadLeft(ANossoNumero, 15, '0')
      end
      else
         raise Exception.Create( ACBrStr('Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"') ) ;
   end;

   Result := ANossoNumero;
end;

function TACBrCaixaEconomica.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras :String;
  CampoLivre,DVCampoLivre, ANossoNumero, ACodCedente : String;
begin

    FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);
    
    ANossoNumero := FormataNossoNumero(ACBrTitulo);
    ACodCedente := DefineCodigoCedente(ACBrTitulo.ACBrBoleto.Cedente);
    {Montando Campo Livre}
    CampoLivre := IfThen(Length(ACodCedente)= 7, ACodCedente,
                  PadLeft(ACodCedente,6,'0') + CalcularDVCedente(ACBrTitulo)) +
                  Copy(ANossoNumero,3,3)  +
                  Copy(ANossoNumero,1,1) + Copy(ANossoNumero,6,3) +
                  Copy(ANossoNumero,2,1) + Copy(ANossoNumero,9,9);

    Modulo.CalculoPadrao;
    Modulo.MultiplicadorFinal   := 2;
    Modulo.MultiplicadorInicial := 9;
    Modulo.Documento := CampoLivre;
    Modulo.Calcular;
    DVCampoLivre := intTostr(Modulo.ModuloFinal);

    if Length(DVCampoLivre) > 1 then
       DVCampoLivre := '0';

    CampoLivre := CampoLivre + DVCampoLivre;
    
    {Codigo de Barras}
    with ACBrTitulo.ACBrBoleto do
    begin
       CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                       '9' +
                       FatorVencimento +
                       IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                       CampoLivre;
    end;

    DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
    Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44);
end;

function TACBrCaixaEconomica.RetornaCodCarteira(const Carteira: string; const ACBrTitulo: TACBrTitulo): integer;

begin
    if (Carteira = 'RG') then
     begin
       if (ACBrTitulo.CarteiraEnvio = tceCedente) then
          Result := 14
       else
          Result := 11;
     end
    else if (Carteira = 'SR') then
     begin
       if (ACBrTitulo.CarteiraEnvio = tceCedente) then
          Result := 24
       else
         Result := 21;
     end
    else
       raise Exception.Create(ACBrStr('Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"'));

end;

function TACBrCaixaEconomica.DefineAceiteImpressao(
  const ACBrTitulo: TACBrTitulo): String;
begin
    case ACBrTitulo.Aceite of
    atSim :
      Result := 'A';
  else
    Result := 'N';
  end;
end;

function TACBrCaixaEconomica.DefineCodigoCedente(const ACBrCedente: TACBrCedente): String;
begin
  if ((fpLayoutVersaoArquivo = 107) and (fpLayoutVersaoLote = 67))
     or (fpLayoutVersaoArquivo = 007) then
    Result := PadLeft(  ACBrCedente.CodigoCedente, 7, '0')
  else
    Result := PadLeft(  ACBrCedente.CodigoCedente, 6, '0');
end;

function TACBrCaixaEconomica.ConverteModalidadeEmCodCarteira(
  const Modalidade: Integer): String;
begin
  case Modalidade of
    11, 14: Result:= 'RG';
    21, 24: Result:= 'SR';
  else
    raise Exception.Create( ACBrStr('Código de Modalidade Inválido para Carteira "RG" ou "SR"') ) ;
  end;

end;

procedure TACBrCaixaEconomica.EhObrigatorioAgenciaDV;
begin
  //sem validação
end;

function TACBrCaixaEconomica.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoSolicitacaoImpressaoTituloConfirmada            : Result := '01';
      toRetornoRegistroConfirmado                              : Result := '02';
      toRetornoRegistroRecusado                                : Result := '03';
      toRetornoTransferenciaCarteiraEntrada                    : Result := '04';
      toRetornoTransferenciaCarteiraBaixa                      : Result := '05';
      toRetornoLiquidado                                       : Result := '06';
      toRetornoRecebimentoInstrucaoConcederDesconto            : Result := '07';
      toRetornoRecebimentoInstrucaoCancelarDesconto            : Result := '08';
      toRetornoBaixado                                         : Result := '09';
      toRetornoRecebimentoInstrucaoConcederAbatimento          : Result := '12';
      toRetornoRecebimentoInstrucaoCancelarAbatimento          : Result := '13';
      toRetornoRecebimentoInstrucaoAlterarVencimento           : Result := '14';
      toRetornoRecebimentoInstrucaoProtestar                   : Result := '19';
      toRetornoRecebimentoInstrucaoSustarProtesto              : Result := '20';
      toRetornoEncaminhadoACartorio                            : Result := '23';
      toRetornoRetiradoDeCartorio                              : Result := '24';
      toRetornoBaixaPorProtesto                                : Result := '25';
      toRetornoInstrucaoRejeitada                              : Result := '26';
      toRetornoAlteracaoUsoCedente                             : Result := '27';
      toRetornoDebitoTarifas                                   : Result := '28';
      toRetornoAlteracaoOutrosDadosRejeitada                   : Result := '30';
      toRetornoConfirmacaoInclusaoBancoSacado                  : Result := '35';
      toRetornoConfirmacaoAlteracaoBancoSacado                 : Result := '36';
      toRetornoConfirmacaoExclusaoBancoSacado                  : Result := '37';
      toRetornoEmissaoBloquetoBancoSacado                      : Result := '38';
      toRetornoManutencaoSacadoRejeitada                       : Result := '39';
      toRetornoEntradaTituloBancoSacadoRejeitada               : Result := '40';
      toRetornoManutencaoBancoSacadoRejeitada                  : Result := '41';
      toRetornoBaixaOuLiquidacaoEstornada                      : Result := '44';
      toRetornoRecebimentoInstrucaoAlterarDados                : Result := '45';
      toRetornoLiquidadoOnLine                                 : Result := '46';
      toRetornoEstornoLiquidacaoOnLine                         : Result := '47';
      toRetornoTituloDDAReconhecidoPagador                     : Result := '51';
      toRetornoTituloDDANaoReconhecidoPagador                  : Result := '52';
      toRetornoTituloDDARecusadoCIP                            : Result := '53';
      toRetornoConfirmacaoAlteracaoValorNominal                : Result := '61';
      toRetornoConfirmacaoAlteracaoValorpercentualMinimoMaximo : Result := '62';
    end;
  end
  else
  begin
    case TipoOcorrencia of
      toRetornoRegistroConfirmado                              : Result := '01';
      toRetornoBaixaManualConfirmada                           : Result := '02';
      toRetornoAbatimentoConcedido                             : Result := '03';
      toRetornoAbatimentoCancelado                             : Result := '04';
      toRetornoVencimentoAlterado                              : Result := '05';
      toRetornoAlteracaoUsoCedente                             : Result := '06';
      toRetornoPrazoProtestoAlterado                           : Result := '07';
      toRetornoPrazoDevolucaoAlterado                          : Result := '08';
      toRetornoDadosAlterados                                  : Result := '09';
      toRetornoAlteracaoReemissaoBloquetoConfirmada            : Result := '10';
      toRetornoAlteracaoOpcaoProtestoParaDevolucaoConfirmada   : Result := '11';
      toRetornoAlteracaoOpcaoDevolucaoParaProtestoConfirmada   : Result := '12';
      toRetornoTituloEmSer                                     : Result := '20';
      toRetornoLiquidado                                       : Result := '21';
      toRetornoLiquidadoEmCartorio                             : Result := '22';
      toRetornoBaixadoPorDevolucao                             : Result := '23';
      toRetornoBaixaPorProtesto                                : Result := '25';
      toRetornoEncaminhadoACartorio                            : Result := '26';
      toRetornoProtestoSustado                                 : Result := '27';
      toRetornoEstornoProtesto                                 : Result := '28';
      toRetornoProtestoOuSustacaoEstornado                     : Result := '29';
      toRetornoRecebimentoInstrucaoAlterarDados                : Result := '30';
      toRetornoTarifaDeManutencaoDeTitulosVencidos             : Result := '31';
      toRetornoOutrasTarifasAlteracao                          : Result := '32';
      toRetornoEstornoBaixaLiquidacao                          : Result := '33';
      toRetornoDebitoTarifas                                   : Result := '34';
      toRetornoRegistroRecusado                                : Result := '99';
    end;
  end;
end;

function TACBrCaixaEconomica.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
  Result := RightStr(ACBrTitulo.ACBrBoleto.Cedente.Agencia,4)+ '/' +
            ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente+ '-' +
                CalcularDVCedente(ACBrTitulo);
end;

function TACBrCaixaEconomica.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
var ANossoNumero : string;
begin
    ANossoNumero := FormataNossoNumero(ACBrTitulo);

    Result := ANossoNumero + '-' + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrCaixaEconomica.GerarRegistroHeader240(NumeroRemessa : Integer): String;
var
  ATipoInscricao: string;
  AMensagemReservada: String;
  ACodCedente: String;
begin

   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      case TipoInscricao of
         pFisica  : ATipoInscricao := '1';
         pJuridica: ATipoInscricao := '2';
      end;

      if ACBrBanco.ACBrBoleto.Homologacao then
        AMensagemReservada := 'REMESSA-TESTE'
      else
        AMensagemReservada := 'REMESSA-PRODUCAO';

      ACodCedente := DefineCodigoCedente(ACBrBanco.ACBrBoleto.Cedente);

          { GERAR REGISTRO-HEADER DO ARQUIVO }

      Result:= IntToStrZero(ACBrBanco.Numero, 3)                + //1 a 3 - Código do banco
               '0000'                                           + //4 a 7 - Lote de serviço
               '0'                                              + //8 - Tipo de registro - Registro header de arquivo
               PadRight('', 9, ' ')                             + //9 a 17 Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                                   + //18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 14, '0')            + //19 a 32 -Número de inscrição do cedente
               PadRight('',20, '0')                             + //33 a 52 - Código do convênio no banco  [ Alterado conforme instruções da CSO Brasília ] 27-07-09
               PadLeft(OnlyNumber(Agencia), 5, '0')             + //53 a 57 - Código da agência do cedente
               PadRight(AgenciaDigito, 1 , '0')                 + //58 - Dígito da agência do cedente
               PadRight(ACodCedente, 7, '0')                    + //59 a 65 - Código Cedente (Código do Convênio no Banco)
               PadRight('', 6, '0')                             + //66 a 71 - Uso Exclusivo CAIXA
               '0'                                              + //72 - Uso Exclusivo CAIXA
               PadRight(Nome, 30, ' ')                          + //73 a 102 - Nome do cedente
               PadRight('CAIXA ECONOMICA FEDERAL', 30, ' ')     + //103 a 132 - Nome do banco
               PadRight('', 10, ' ')                            + //133 a 142 - Uso exclusivo FEBRABAN/CNAB
               '1'                                              + //143 - Código de Remessa (1) / Retorno (2)
               FormatDateTime('ddmmyyyy', Now)                  + //144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now)                    + //152 a 157 - Hora de geração do arquivo
               PadLeft(IntToStr(NumeroRemessa), 6, '0')         + //158 a 163 - Número seqüencial do arquivo
               PadLeft(IntToStr(fpLayoutVersaoArquivo), 3, '0') + //164 a 166 - Número da versão do layout do arquivo
               PadRight('',  5, '0')                            + //167 a 171 - Densidade de gravação do arquivo (BPI)
               Space(20)                                        + //172 a 191 - Uso reservado do banco
               PadRight(AMensagemReservada, 20, ' ')            + //192 a 211 - Uso reservado da empresa
               PadRight('', 4, ' ')                             + //212 a 215 - Versao Aplicativo Caixa
               PadRight('', 25, ' ');                             //216 a 240 - Uso Exclusivo FEBRABAN / CNAB

          { GERAR REGISTRO HEADER DO LOTE }

      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)            +//1 a 3 - Código do banco
               '0001'                                       +//4 a 7 - Lote de serviço
               '1'                                          +//8 - Tipo de registro - Registro header de arquivo
               'R'                                          +//9 - Tipo de operação: R (Remessa) ou T (Retorno)
               '01'                                         +//10 a 11 - Tipo de serviço: 01 (Cobrança)
               '00'                                         +//12 a 13 - Forma de lançamento: preencher com ZEROS no caso de cobrança
               PadLeft(IntToStr(fpLayoutVersaoLote), 3, '0')+//14 a 16 - Número da versão do layout do lote
               ' '                                          +//17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                               +//18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 15, '0')        +//19 a 33 -Número de inscrição do cedente
               PadRight(ACodCedente, 7, '0')                +//34 a 40 - Código do convênio no banco (código do cedente)
               PadRight('', 13, '0')                        +//41 a 53 - Uso Exclusivo Caixa
               PadLeft(OnlyNumber(Agencia), 5 , '0')        +//54 a 58 - Dígito da agência do cedente
               PadRight(AgenciaDigito, 1 , '0')             +//59 - Dígito da agência do cedente
               IfThen(Length(ACodCedente)= 6, ACodCedente,
                            PadRight('',6,'0') )            +//60 a 65 - Código do convênio no banco (código do cedente)
               PadRight('',7,'0')                           +//66 a 72 - Código do Modelo Personalizado (Código fornecido pela CAIXA/gráfica, utilizado somente quando o modelo do bloqueto for personalizado)
               '0'                                          +//73 - Uso Exclusivo Caixa
               PadRight(Nome, 30, ' ')                      +//74 a 103 - Nome do cedente
               PadRight('', 40, ' ')                        +//104 a 143 - Mensagem 1 para todos os boletos do lote
               PadRight('', 40, ' ')                        +//144 a 183 - Mensagem 2 para todos os boletos do lote
               PadLeft(IntToStr(NumeroRemessa), 8, '0')     +//184 a 191 - Número do arquivo
               FormatDateTime('ddmmyyyy', Now)              +//192 a 199 - Data de geração do arquivo
               PadRight('', 8, '0')                         +//200 a 207 - Data do crédito - Só para arquivo retorno
               PadRight('', 33, ' ');                        //208 a 240 - Uso exclusivo FEBRABAN/CNAB
   end;
end;

procedure TACBrCaixaEconomica.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList);
var
  wLinha: String;
  ACodCedente: String;
begin

   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      ACodCedente := DefineCodigoCedente(ACBrBanco.ACBrBoleto.Cedente);
      { GERAR REGISTRO-HEADER DO ARQUIVO }

      wLinha:= '0'                                     + // 1 ate 1     - Código Identificador do tipo de Registro no Arquivo
               '1'                                     + // 2 ate 2     - ID do Arquivo( 1 - Remessa)
               'REMESSA'                               + // 3 ate 9     - Literal de Remessa
               '01'                                    + // 10 ate 11   - Código Identificador do Tipo de Serviço
               PadRight('COBRANCA', 15 )               + // 12 ate 26   - Literal correspondente ao códigode Serviço (COBRANÇA)
               RightStr(Agencia,4)                     + // 27 ate 30   - código agência
               PadRight(ACodCedente, 7, ' ')           + // 31 ate 37   - Código Identificador da Empresa na CAIXA (CEDENTE)
               Space(9)                                + // 38 ate 46   - brancos
               PadRight(Nome, 30)                      + // 47 ate 76   - Nome da Empresa
               '104'                                   + // 77 ate 79   - Código do Banco (104)
               PadRight('C ECON FEDERAL', 15)          + // 80 ate 94   - Nome do Banco(C ECON FEDERAL)
               FormatDateTime('ddmmyy',Now)            + // 95 ate 100  - Data de geração do arquivo
               ifThen((((fpLayoutVersaoArquivo = 107)
                     and (fpLayoutVersaoLote = 67))
                     or (fpLayoutVersaoArquivo = 007)), '007', space(3)) + // 101 ate 103 - Nº da Versão do Layout
               space(286)                              + // 104 ate 389 - Brancos
               IntToStrZero(NumeroRemessa,5)           + // 390 ate 394 - Número sequencial do Arquivo de Remessa
               IntToStrZero(1,6);                        // 395 ate 400 - Número sequencial do Registrono Arquivo
      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

function TACBrCaixaEconomica.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
  ATipoOcorrencia, ATipoBoleto, ADataMoraJuros, AModalidade        : String;
  ADataDesconto, ADataMulta, ANossoNumero, ATipoAceite, AEspecieDoc: String;
  ACodigoDesconto                                                  : String;
  ACodCedente: String;

  function MontarInstrucoes2: string;
  begin
    Result := '';
    with ACBrTitulo do
    begin
      if (Mensagem.Count <= 2) then
      begin
        if (Mensagem.Count = 2) then
          Result := Copy(PadRight(Mensagem[0] +' / '+ Mensagem[1], 140, ' '), 1, 140)
        else
          Result := Copy(PadRight(Mensagem[0], 140, ' '), 1, 140);

        Exit;
      end;

      if (Mensagem.Count >= 3) then
      begin
        Result := Copy(PadRight(Mensagem[2], 40, ' '), 1, 40);
      end;

      if (Mensagem.Count >= 4) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[3], 40, ' '), 1, 40)
      end;

      if (Mensagem.Count >= 5) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[4], 40, ' '), 1, 40)
      end;

      if (Mensagem.Count >= 6) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[5], 40, ' '), 1, 40)
      end;

      if (Mensagem.Count >= 7) then
      begin
        Result := Result +
                  Copy(PadRight(Mensagem[6], 40, ' '), 1, 40)
      end;

      // Acertar a quantidade de caracteres
      Result := PadRight(Result, 200);

    end;
  end;

begin
   with ACBrTitulo do
   begin
      {Pegando a Modalidade}
        AModalidade:= RetornaModalidade(ACBrTitulo);

      if ( Trim(ACBrTitulo.NossoNumero) <> '' ) then
        ANossoNumero := FormataNossoNumero(ACBrTitulo)
      else
        ANossoNumero := '';  

      {SEGMENTO P}

      {Pegando o Tipo de Ocorrencia}

      ATipoOcorrencia := TipoOcorrenciaToCodRemessa(OcorrenciaOriginal.Tipo);

      { Pegando o Aceite do Titulo }
      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      if AnsiSameText(EspecieDoc, 'CH') then
        AEspecieDoc := '01'
      else if AnsiSameText(EspecieDoc, 'DM') then
        AEspecieDoc := '02'
      else if AnsiSameText(EspecieDoc, 'DMI') then
        AEspecieDoc := '03'
      else if AnsiSameText(EspecieDoc, 'DS') then
        AEspecieDoc := '04'
      else if AnsiSameText(EspecieDoc, 'DSI') then
        AEspecieDoc := '05'
      else if AnsiSameText(EspecieDoc, 'DR') then
        AEspecieDoc := '06'
      else if AnsiSameText(EspecieDoc, 'LC') then
        AEspecieDoc := '07'
      else if AnsiSameText(EspecieDoc, 'NCC') then
        AEspecieDoc := '08'
      else if AnsiSameText(EspecieDoc, 'NCE') then
        AEspecieDoc := '09'
      else if AnsiSameText(EspecieDoc, 'NCI') then
        AEspecieDoc := '10'
      else if AnsiSameText(EspecieDoc, 'NCR') then
        AEspecieDoc := '11'
      else if AnsiSameText(EspecieDoc, 'NP') then
        AEspecieDoc := '12'
      else if AnsiSameText(EspecieDoc, 'NPR') then
        AEspecieDoc := '13'
      else if AnsiSameText(EspecieDoc, 'TM') then
        AEspecieDoc := '14'
      else if AnsiSameText(EspecieDoc, 'TS') then
        AEspecieDoc := '15'
      else if AnsiSameText(EspecieDoc, 'NS') then
        AEspecieDoc := '16'
      else if AnsiSameText(EspecieDoc, 'RC') then
        AEspecieDoc := '17'
      else if AnsiSameText(EspecieDoc, 'FAT') then
        AEspecieDoc := '18'
      else if AnsiSameText(EspecieDoc, 'ND') then
        AEspecieDoc := '19'
      else if AnsiSameText(EspecieDoc, 'AP') then
        AEspecieDoc := '20'
      else if AnsiSameText(EspecieDoc, 'ME') then
        AEspecieDoc := '21'
      else if AnsiSameText(EspecieDoc, 'PC') then
        AEspecieDoc := '22'
      else if AnsiSameText(EspecieDoc, 'NF') then
        AEspecieDoc := '23'
      else if AnsiSameText(EspecieDoc, 'DD') then
        AEspecieDoc := '24'
      else if AnsiSameText(EspecieDoc, 'CPR') then
        AEspecieDoc := '25'
      else
        AEspecieDoc := '99';

      {Pegando Tipo de Boleto} //Quem emite e quem distribui o boleto?
      case ACBrBoleto.Cedente.ResponEmissao of
         tbBancoEmite      : ATipoBoleto := '1' + '1';
         tbCliEmite        : ATipoBoleto := '2' + '0';
         tbBancoReemite    : ATipoBoleto := '4' + '1';
         tbBancoNaoReemite : ATipoBoleto := '5' + '0';
      end;

      {Mora Juros}
      if (ValorMoraJuros > 0) then
       begin
         if DataMoraJuros <> 0 then
            ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
         else
            ADataMoraJuros := PadRight('', 8, '0');
       end
      else
         ADataMoraJuros := PadRight('', 8, '0');

      {Descontos}
      if (ValorDesconto > 0) then
       begin
         if (DataDesconto > 0) then
            ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
         else
            ADataDesconto := PadRight('', 8, '0');
        
         case TipoDesconto of
           tdValorFixoAteDataInformada : ACodigoDesconto := '1';
           tdPercentualAteDataInformada: ACodigoDesconto := '2';
	 else
	   ACodigoDesconto := '1';
         end; 
       end
      else begin
        ADataDesconto := PadRight('', 8, '0');
        ACodigoDesconto := '0';
      end;

      {Multa}
      if (PercentualMulta > 0) then
        ADataMulta := IfThen(DataMulta > 0,
                             FormatDateTime('ddmmyyyy', DataMulta),
                             FormatDateTime('ddmmyyyy', Vencimento + 1))
      else
        ADataMulta := PadLeft('', 8, '0');

      ACodCedente := DefineCodigoCedente(ACBrTitulo.ACBrBoleto.Cedente);

      fValorTotalDocs:= fValorTotalDocs  + ValorDocumento;
      Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //1 a 3 - Código do banco
               '0001'                                                     + //4 a 7 - Lote de serviço
               '3'                                                        + //8 - Tipo do registro: Registro detalhe
               IntToStrZero(fQtRegLote + 1,5)                             + //9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'P'                                                        + //14 - Código do segmento do registro detalhe
               ' '                                                        + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                            + //16 a 17 - Código de movimento
               PadLeft(OnlyNumber(ACBrBoleto.Cedente.Agencia), 5, '0')    + //18 a 22 - Agência mantenedora da conta
               PadRight(ACBrBoleto.Cedente.AgenciaDigito, 1 , '0')        + //23 -Dígito verificador da agência
               PadRight(ACodCedente, 7, '0')                              + //24 a 30 - Código do Convênio no Banco (Codigo do cedente)
               PadRight('', 10, '0')                                      + //31 a 40 - Uso Exclusivo da CAIXA
               PadRight(Copy(AModalidade,1,2), 2, '0')                    + //41 a 42 - Modalidade da Carteira
               PadLeft(Copy(ANossoNumero,3,17), 15, '0')                  + //43 a 57 - Nosso número - identificação do título no banco
               '1'                                                        + //58 - Cobrança Simples
               '1'                                                        + //59 - Forma de cadastramento do título no banco: com cadastramento  1-cobrança Registrada
               '2'                                                        + //60 - Tipo de documento: Tradicional
               ATipoBoleto                                                + //61 e 62(juntos)- Quem emite e quem distribui o boleto?
               PadRight(NumeroDocumento, 11, ' ')                         + //63 a 73 - Número que identifica o título na empresa
               PadRight('', 4, ' ')                                       + //74 a 77 - Uso Exclusivo Caixa
               FormatDateTime('ddmmyyyy', Vencimento)                     + //78 a 85 - Data de vencimento do título
               IntToStrZero( round( ValorDocumento * 100), 15)            + //86 a 100 - Valor nominal do título
               PadRight('', 5, '0')                                       + //101 a 105 - Agência cobradora. Se ficar em branco, a caixa determina automaticamente pelo CEP do sacado
               '0'                                                        + //106 - Dígito da agência cobradora
               PadRight(AEspecieDoc, 2)                                   + // 107 a 108 - Espécie do documento
               ATipoAceite                                                + //109 - Identificação de título Aceito / Não aceito
               FormatDateTime('ddmmyyyy', DataDocumento)                  + //110 a 117 - Data da emissão do documento
               IfThen( (ValorMoraJuros > 0) and (CodigoMora= ''), '1', PadRight(CodigoMora, 1, '3') )   + //118 - Código de juros de mora: Valor por dia
               ADataMoraJuros                                             + //119 a 126 - Data a partir da qual serão cobrados juros
               IfThen(ValorMoraJuros > 0, IntToStrZero( round(ValorMoraJuros * 100), 15),
                      PadRight('', 15, '0'))                                                   + //127 a 141 - Valor de juros de mora por dia
               ACodigoDesconto                                            + //142 - Código de desconto: Valor fixo até a data informada
               ADataDesconto                                              + //143 a 150 - Data do desconto
               IfThen(ValorDesconto > 0, IntToStrZero( round(ValorDesconto * 100), 15),
                      PadRight('', 15, '0'))                                                   + //151 a 165 - Valor do desconto por dia
               IntToStrZero( round(ValorIOF * 100), 15)                   + //166 a 180 - Valor do IOF a ser recolhido
               IntToStrZero( round(ValorAbatimento * 100), 15)            + //181 a 195 - Valor do abatimento
               PadRight(IfThen(SeuNumero<>'',SeuNumero,NumeroDocumento), 25, ' ') + //196 a 220 - Identificação do título na empresa
               IfThen((DataProtesto <> 0) and (DiasDeProtesto > 0), '1', '3') + //221 - Código de protesto: Protestar em XX dias corridos
               IfThen((DataProtesto <> 0) and (DiasDeProtesto > 0),
                    PadLeft(IntToStr(DiasDeProtesto), 2, '0'), '00') + //222 a 223 - Prazo para protesto (em dias corridos)
               IfThen(((DataProtesto = 0) or (DataProtesto <= Vencimento))
                      or ((DataBaixa <> 0) and (DataBaixa > Vencimento)), '1', '2') + //224 - Código para baixa/devolução: Não baixar/não devolver
               IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento),
                 PadLeft(IntToStr(DaysBetween(DataBaixa, Vencimento)), 3, '0'), '000') + //225 a 227 - Prazo para baixa/devolução (em dias corridos)

               '09'                                                       + //228 a 229 - Código da moeda: Real
               PadRight('', 10 , '0')                                         + //230 a 239 - Uso Exclusivo CAIXA
               ' ';                                                         //240 - Uso exclusivo FEBRABAN/CNAB

      Inc(fQtRegLote);

      {SEGMENTO Q}
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                                       + //1 a 3 - Código do banco
               '0001'                                                                  + //4 a 7 - Número do lote
               '3'                                                                     + //8 - Tipo do registro: Registro detalhe
               IntToStrZero(fQtRegLote + 1,5)                                          + //9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'Q'                                                                     + //14 - Código do segmento do registro detalhe
               ' '                                                                     + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                                         + //16 a 17 - Código de movimento
                   {Dados do sacado}
               IfThen(Sacado.Pessoa = pJuridica,'2','1')                               + //18 - Tipo inscricao
               PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                            + //19 a 33 - Número de Inscrição
               PadRight(Sacado.NomeSacado, 40, ' ')                                    + //34 a 73 - Nome sacado
               PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' ' + 
                        Sacado.Complemento , 40, ' ')                                  + //74 a 113 - Endereço
               PadRight(Sacado.Bairro, 15, ' ')                                        + // 114 a 128 - bairro sacado
               PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                                 + // 129 a 133 e 134 a 136- cep sacado prefixo e sufixo sem o traço"-" somente numeros
               PadRight(Sacado.Cidade, 15, ' ')                                        + // 137 a 151 - cidade sacado
               PadRight(Sacado.UF, 2, ' ')                                             + // 152 a 153 - UF sacado
               {Dados do sacador/avalista}
               IfThen(EstaVazio(Sacado.SacadoAvalista.NomeAValista),
                      '0',
                      IfThen(Sacado.SacadoAvalista.Pessoa = pJuridica,
                             '2',
                             '1'
                      )
               )                                                                       + // 154 a 157 - Tipo de Inscrição
               PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 15, '0')             + // 155 a 169 - Número de inscrição
               PadRight(Sacado.SacadoAvalista.NomeAValista, 40, ' ')                   + // 170 a 209 - Nome do sacador/avalista
               IfThen(fpLayoutVersaoArquivo = 50, Space(3), '000')                     + // 210 a 212 - Uso exclusivo FEBRABAN/CNAB
               Space(20)                                                               + // 213 a 232 - Uso exclusivo FEBRABAN/CNAB
               Space(8);                                                                 // 233 a 240 - Uso exclusivo FEBRABAN/CNAB

      Inc(fQtRegLote);

 {SEGMENTO R}
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                                           + //   1 a 3   - Código do banco
               '0001'                                                                      + //   4 a 7   - Número do lote
               '3'                                                                         + //   8 a 8   - Tipo do registro: Registro detalhe
               IntToStrZero(fQtRegLote + 1,5)                                              + //   9 a 13  - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'R'                                                                         + //  14 a 14  - Código do segmento do registro detalhe
               ' '                                                                         + //  15 a 15  - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                                             + //  16 a 17  - Tipo Ocorrencia
               PadLeft('', 1,  '0')                                                           + //  18 a 18  - Codigo do Desconto 2
               PadLeft('', 8,  '0')                                                           + //  19 a 26  - Data do Desconto 2
               PadLeft('', 15, '0')                                                           + //  27 a 41  - Valor/Percentual a ser concedido
               PadLeft('', 1,  '0')                                                           + //  42 a 42  - Código do Desconto 3
               PadLeft('', 8,  '0')                                                           + //  43 a 50  - Data do Desconto 3
               PadLeft('', 15, '0')                                                           + //  51 a 65  - Valor/Percentual a ser concedido
               IfThen((PercentualMulta > 0),
                       IfThen(MultaValorFixo,'1','2'), '0')                                   + //  66 a 66  - Código da Multa
               ADataMulta                                                                     + //  67 a 74  - Data da Multa
               IfThen(PercentualMulta > 0, IntToStrZero(round(PercentualMulta * 100), 15),
                      PadRight('', 15, '0'))                                                   + //  75 a 89  - Valor/Percentual a ser aplicado
               PadRight('', 10, ' ')                                                           + //  90 a 99  - Informação ao Sacado
               PadRight('', 40, ' ')                                                           + // 100 a 139 - Mensagem 3
               PadRight('', 40, ' ')                                                           + // 140 a 179 - Mensagem 4
               PadRight(Sacado.Email, 50, ' ')                                                 + // 180 a 229 - Email do Sacado P/ Envio de Informacoes
               PadRight('', 11, ' ');                                                            // 230 a 240 - Uso Exclusivo Febraban/CNAB

    Inc(fQtRegLote);

    {SEGMENTO S}
    if (Mensagem.Count > 0) then
    begin
      Result := Result + #13#10 +
                IntToStrZero(ACBrBanco.Numero, 3)                                           + //   1 a 3   - Código do banco
                '0001'                                                                      + // 004 - 007 / Numero do lote remessa
                '3'                                                                         + // 008 - 008 / Tipo de registro
                IntToStrZero(fQtRegLote + 1,5)                                              + //   9 a 13  - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
                'S'                                                                         + // 014 - 014 / Cód. Segmento do registro detalhe
                Space(1)                                                                    + // 015 - 015 / Reservado (uso Banco)
                ATipoOcorrencia                                                             + // 016 - 017 / Código de movimento remessa
                ifthen( (Mensagem.Count <= 2), '2', '3' )                                   + // 018 - 018 / Identificação da impressão
                ifthen( (Mensagem.Count <= 2), '00', '' )                                   + // 019 - 020 / Reservado (uso Banco) para tipo de impressão 1 e 2
                MontarInstrucoes2                                                           + // 019 - 058 / Mensagem 5
                                                                                              // 059 - 098 / Mensagem 6
                                                                                              // 099 - 138 / Mensagem 7
                                                                                              // 139 - 178 / Mensagem 8
                                                                                              // 179 - 218 / Mensagem 9
                ifthen( (Mensagem.Count <= 2), '00' + Space(78) ,Space(22));                  // 219 - 240 / Reservado (uso Banco) para tipo de impressão 3

      Inc(fQtRegLote);
    end;                                                                                       // 161 - 240 / Reservado (uso Banco) para tipo de impressão 1 e 2
    {SEGMENTO S - FIM}
      end;
end;

procedure TACBrCaixaEconomica.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  ATipoOcorrencia, ATipoBoleto, ADataMoraJuros, AModalidade  :String;
  aDataDesconto, ANossoNumero, ATipoAceite, ATipoEspecieDoc  :String;
  ATipoSacado, ATipoCendente, wLinha                         :String;
  wCarteira                                                  :Integer;
  ACodigoDesconto, ADataMulta : String;
  ACodCedente : String;

  function DoMontaInstrucoes1: string;
  begin
     Result := '';
     with ACBrTitulo, ACBrBoleto do
     begin

        if Mensagem.Count < 1 then
        begin
           Result := '';
           Exit;
        end;

        Result := sLineBreak +
                  '2'                                                            + //    1 até 1 - Identificador do Tipo de Registro do Arquivo
                  ATipoCendente                                                  + //    2 até 3 - Tipo de inscrição da empresa 01-CPF / 02-CNPJ
                  PadLeft(OnlyNumber(ACBrBoleto.Cedente.CNPJCPF),14,'0')         + //   4 até 17 - Numero de Inscrição Empresa
                  RightStr(OnlyNumber(ACBrBoleto.Cedente.Agencia), 4 )           + //  18 até 21 - Código da Agência
                  PadRight(ACodCedente, 7, ' ')                                  + //  22 até 28 - Código do Cedente
                  Space(3)                                                       + //  29 até 31 - Uso Exclusivo CAIXA
                  Space(25)                                                      + //  32 até 56 - Campo em Branco
                  PadRight(Copy(AModalidade,1,2), 2, '0')                        + //  57 até 58 - Modalidade
                  PadLeft(Copy(ANossoNumero, 3, 15), 15, '0')                    + //  59 até 73 - Nosso Numero
                  Space(33)                                                      + //  74 até 106 - Campos em Branco
                  '01'                                                           + // 107 até 108 - Código Carteira //PadLeft(IntToStr(RetornaCodCarteira(Carteira)),2,'0')
                  ATipoOcorrencia                                                + // 109 até 110 - Código da ocorrencia
                  Space(29)                                                      + // 111 ate 139 - Uso Exclusivo CAIXA
                  '104';                                                           // 140 até 142 - Código de compensação da CAIXA

        if (Mensagem.Count >= 1) then
           Result := Result +
                     Copy(PadRight(Mensagem[0], 40, ' '), 1, 40)                   // 143 até 182 - CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 40, ' ');                               // CONTEÚDO DO RESTANTE DAS LINHAS

        if (Mensagem.Count >= 2) then
           Result := Result +
                     Copy(PadRight(Mensagem[1], 40, ' '), 1, 40)                   // 183 até 222 - CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 40, ' ');                               // CONTEÚDO DO RESTANTE DAS LINHAS

        if (Mensagem.Count >= 3) then
           Result := Result +
                     Copy(PadRight(Mensagem[2], 40, ' '), 1, 40)                   // 223 até 262 - CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 40, ' ');                               // CONTEÚDO DO RESTANTE DAS LINHAS

        if (Mensagem.Count >= 4) then
           Result := Result +
                     Copy(PadRight(Mensagem[3], 40, ' '), 1, 40)                   // 263 até 302 - CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 40, ' ');                               // CONTEÚDO DO RESTANTE DAS LINHAS

        if (Mensagem.Count >= 5) then
           Result := Result +
                     Copy(PadRight(Mensagem[4], 40, ' '), 1, 40)                   // 303 até 342 - CONTEÚDO DA 5ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 40, ' ');                               // CONTEÚDO DO RESTANTE DAS LINHAS

        if (Mensagem.Count >= 6) then
           Result := Result +
                     Copy(PadRight(Mensagem[5], 40, ' '), 1, 40)                   // 343 até 382 - CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
        else
           Result := Result + PadRight('', 40, ' ');                               // CONTEÚDO DO RESTANTE DAS LINHAS

        Result := Result                                              +
                  Space(12)                                           +            // 383 até 394 - Uso Exclusivo da Caixa
                  IntToStrZero(aRemessa.Count + 2, 6);                             // 395 até 400 - Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
     end;
  end;

begin
   with ACBrTitulo do
   begin
      {Pegando a Modalidade}
      AModalidade:= RetornaModalidade(ACBrTitulo);

      wCarteira:= RetornaCodCarteira(Carteira, ACBrTitulo);
      if (((wCarteira = 11) or (wCarteira= 31) or (wCarteira = 51)) or
         ((wCarteira = 12) or (wCarteira = 15) or (wCarteira = 17)) and
          (ACBrBoleto.Cedente.ResponEmissao <>  tbCliEmite)) then
        ANossoNumero := '00000000000000000000'
      else
        ANossoNumero := FormataNossoNumero(ACBrTitulo);

      {Mora Juros}
      if (ValorMoraJuros > 0) then
       begin
         if (DataMoraJuros > 0) and (DataMoraJuros >= 30/12/2000) then
            ADataMoraJuros := FormatDateTime('ddmmyy', DataMoraJuros)
         else
            ADataMoraJuros := PadLeft('', 6, '0');
       end
      else
         ADataMoraJuros := PadLeft('', 6, '0');

      {Multa}
      if (PercentualMulta > 0) then
        ADataMulta := IfThen(DataMulta > 0,
                             FormatDateTime('ddmmyy', DataMulta),
                             ADataMoraJuros)
      else
        ADataMulta := PadLeft('', 6, '0');

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : ATipoOcorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : ATipoOcorrencia := '03'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : ATipoOcorrencia := '04'; {Cancelamento de Abatimento}
         toRemessaAlterarVencimento              : ATipoOcorrencia := '05'; {Alteração de vencimento}
         toRemessaAlterarUsoEmpresa              : ATipoOcorrencia := '06'; {Alteração do uso da Empresa}
         toRemessaAlterarPrazoProtesto           : ATipoOcorrencia := '07'; {Alteração do Prazo de Protesto}
         toRemessaAlterarPrazoDevolucao          : ATipoOcorrencia := '08'; {Alteração do Prazo de Devolução}
         toRemessaAlterarOutrosDados             : ATipoOcorrencia := '09'; {Alteração de outros dados}
         toRemessaAlterarDadosEmissaoBloqueto    : ATipoOcorrencia := '10'; {Alt de dados c/ emissão / emissão de bloqueto}
         toRemessaAlterarProtestoDevolucao       : ATipoOcorrencia := '11'; {Alteração da opção de Protesto para Devolução}
         toRemessaAlterarDevolucaoProtesto       : ATipoOcorrencia := '12'; {Alteração da opção de Devolução para Protesto}
         toRemessaAlterarValorTitulo             : ATipoOcorrencia := '13'; {Alteração do Valor Nominal do Título - 19/08/2021}
      else
         ATipoOcorrencia := '01';                                      {Remessa}
      end;

      { Pegando o Aceite do Titulo }
      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      { Pegando o tipo de EspecieDoc }
      if EspecieDoc = 'DM' then
         ATipoEspecieDoc   := '01'      // Duplicata Mercantil
      else if EspecieDoc = 'NP' then
         ATipoEspecieDoc   := '02'      // Nota Promissória
      else if EspecieDoc = 'DS' then
         ATipoEspecieDoc   := '03'      // Duplicata de Prestação de Serviços
      else if EspecieDoc = 'NS' then
         ATipoEspecieDoc   := '05'      // Nota de Seguro
      else if EspecieDoc = 'LC' then
         ATipoEspecieDoc   := '06'      // Letra de Câmbio
      else if EspecieDoc = 'OU' then
         ATipoEspecieDoc   := '09'     ;// Outros

      case ACBrBoleto.Cedente.ResponEmissao of
         tbBancoEmite      : ATipoBoleto := '1' + '1';
         tbCliEmite        : ATipoBoleto := '2' + '0';
      end;

      if (DataProtesto > 0) and (DataProtesto > Vencimento) then
        Instrucao1:= '01'    // Protestar
      else
        Instrucao1:='02'; //Devolver (Não Protestar)

      Instrucao2  := '00';  //Registro Detalhe Tipo 1: Campo 25.1 - Posição 159 à 160 – Segunda Instrução de Cobrança: Inserir 00



      {Descontos}
      if ValorDesconto > 0 then
      begin
         if DataDesconto > EncodeDate(2000,01,01) then
            aDataDesconto := FormatDateTime('ddmmyy',DataDesconto)
         else
            aDataDesconto := '777777';

         case TipoDesconto of
           tdValorFixoAteDataInformada : ACodigoDesconto := '1';
           tdPercentualAteDataInformada: ACodigoDesconto := '2';
           tdValorAntecipacaoDiaCorrido: ACodigoDesconto := '3';
           tdValorAntecipacaoDiaUtil   : ACodigoDesconto := '4';
           tdPercentualSobreValorNominalDiaCorrido: ACodigoDesconto := '5';
           tdPercentualSobreValorNominalDiaUtil: ACodigoDesconto := '6';
	 else
	   ACodigoDesconto := '1';
         end;
      end
      else begin
        aDataDesconto := PadRight('', 6, '0');
        ACodigoDesconto := '0';
      end;

      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : ATipoSacado := '01';
         pJuridica : ATipoSacado := '02';
      else
         ATipoSacado := '02';
      end;

      {Pegando Tipo de Cedente}
      case ACBrBoleto.Cedente.TipoInscricao of
         pFisica   : ATipoCendente := '01';
         pJuridica : ATipoCendente := '02';
      end;

      {Codigo do Cedente}
      ACodCedente := DefineCodigoCedente(ACBrTitulo.ACBrBoleto.Cedente);

      with ACBrBoleto do
      begin
         wLinha:= '1'                                                              + //  1 até 1   -  ID Registro - Preencher com ‘1'
                  ATipoCendente                                                    + //  2 até 3   -  Tipo de inscrição da empresa 01-CPF / 02-CNPJ
                  PadLeft(OnlyNumber(ACBrBoleto.Cedente.CNPJCPF),14,'0')           + //  4  até 17 - Inscrição da empresa
                  ifthen(Length(ACodCedente) > 6, '000',
                         RightStr(OnlyNumber(ACBrBoleto.Cedente.Agencia), 4 ))     + // 18 até 21  - Código da Agência
                  ifthen(Length(ACodCedente) > 6, PadRight(ACodCedente, 7, '0'),
                         ACodCedente )                                             + // 22 até 27  - Código do Cedente
                  ATipoBoleto                                                      + // 28 a 29    - Quem emite e quem distribui
                  '00'                                                             + // 30 a 31    - Comissão de permanência - informar 00
                  PadRight(ACBrTitulo.SeuNumero, 25, ' ')                          + // 32 até 56  - Seu numero
                  PadRight(Copy(AModalidade,1,2), 2, '0')                          + // 57 até 58  - Modalidade identificação
                  PadLeft(Copy(ANossoNumero, 3, 15), 15, '0')                      + // 59 até 73  - Nosso Numero
                  Space(3)                                                         + // 74 Até 76  - Brancos
                  Space(1)                                                         + //77 até 77 - Campo em branco
                  ADataMoraJuros                                                   + //78 até 83 - Data Inicio de Juros
                  ACodigoDesconto                                                  + //84 até 84 - Código do Tipo de Desconto
                  Space(22)                                                        + //85 até 106 - Campos em Branco
                  '01'                                                             + //107 até 108 - Código Carteira //PadLeft(IntToStr(RetornaCodCarteira(Carteira)),2,'0')
                  ATipoOcorrencia                                                  + //109 até 110 - Código da ocorrencia
                  PadRight(NumeroDocumento, 10, ' ')                               + //111 ate 120 - Seu Numero - Nr. titulo dado pelo cedente
                  FormatDateTime('ddmmyy', Vencimento )                            + //121 ate 126 -  Data de vencimento
                  IntToStrZero(Round(ValorDocumento * 100 ), 13)                   + // 127 ate 139 - Valor do titulo
                  '104'                                                            + // 140 até 142 - Código de compensação da CAIXA
                  '00000'                                                          + // 143 até 147 - Agencia Cobradora
                  PadLeft(ATipoEspecieDoc, 2, '0')                                 + // 148 até 149 - Espécie
                  ATipoAceite                                                      + // 150 até  150  A- aceito / N- não aceito
                  FormatDateTime('ddmmyy', DataDocumento )                         + //151 até 156 - Data de Emissão
                  PadLeft(Instrucao1, 2, '0')                                      + //157 até 158 - Primeira instrução de Cobrança
                  PadLeft(Instrucao2, 2, '0')                                      + //159 até 160 - Primeira instrução de Cobrança
                  IntToStrZero(round(ValorMoraJuros * 100 ), 13)                   + //161 até 173 Juros de mora por dia
                  aDataDesconto                                                    + //174 até 179 Data limite para concessao de desconto
                  IntToStrZero(round(ValorDesconto * 100), 13)                     + //180 até 192  Valor do desconto
                  IntToStrZero(round(ValorIOF * 100 ), 13)                         + //193 até 205 Valor do IOF
                  IntToStrZero(round(ValorAbatimento * 100 ), 13)                  + //206 até 218 Valor do abatimento permitido
                  ATipoSacado                                                      + //219 até 220 "01" - CPF / "02"- CGC
                  PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')                       + // 221 até 234 CNPJ ou CPF do sacado
                  PadRight(Sacado.NomeSacado, 40)                                  + // 235 até 274 Nome do sacado
                  PadRight(trim(Sacado.Logradouro) + ', ' +
                           trim(Sacado.Numero) + ' ' +
                           trim(Sacado.Complemento), 40) +                           //275 até 314 - Endereço do sacado
                  PadRight(Sacado.Bairro, 12)                                      + //315 até  326 - Bairro Sacado
                  PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                          + // 327 até 334 - CEP do endereço do sacado
                  PadRight(trim(Sacado.Cidade), 15)                                + // 335 até 349  - Cidade do sacado
                  PadRight(Sacado.UF, 2 )                                          + // 350 até 351  - UF da cidade do sacado
                  ADataMulta                                                       + //352 até 357  - Data Multa
                  IntToStrZero(round((ValorDocumento* (PercentualMulta*100) )/100), 10)    + //358 até 367 - Valor nominal da multa
                  PadRight(Sacado.SacadoAvalista.NomeAvalista, 22)                 + // 368 até 389 - Nome do Sacador Avalista
                  PadLeft(Instrucao3, 2, '0')                                      + // 390  391 - Terceira instrução de Cobrança Default '00'
                   IfThen((DataProtesto > 0) and (DataProtesto > Vencimento),
                             PadLeft(IntToStr(DaysBetween(DataProtesto,Vencimento)), 2, '0'),
                          IfThen( (DataBaixa > 0) and (DataBaixa > Vencimento),
                             PadLeft(IntToStr(DaysBetween(DataBaixa,Vencimento)), 2, '0'),
                             '99')
                          )                                                          + //392 até 393 - Quantidade de dias para início da ação de protesto ou devolução do Título
                   '1';                                                              // 394 até 394 - Código da moeda: Real

         wLinha := wLinha + IntToStrZero(aRemessa.Count + 1, 6 );                    // 395 até 400
         wLinha := wLinha + DoMontaInstrucoes1;
         aRemessa.Text := aRemessa.Text + UpperCase(wLinha);

      end;
   end;
end;

function TACBrCaixaEconomica.GerarRegistroTrailler240( ARemessa : TStringList ): String;
var
  wQTDTitulos: Integer;
begin

   wQTDTitulos := ARemessa.Count - 1;
   {REGISTRO TRAILER DO LOTE}
   Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
            '0001'                                                     + //Lote de Serviço
            '5'                                                        + //Tipo do registro: Registro trailer do lote
            Space(9)                                                   + //Uso exclusivo FEBRABAN/CNAB
            IntToStrZero((fQtRegLote + 2 ), 6)                         + //Quantidade de Registro no Lote (Registros P,Q,R, header e trailer do lote)
            IntToStrZero((wQTDTitulos), 6)                             + //Quantidade títulos em cobrança
            IntToStrZero( round( fValorTotalDocs * 100), 17)           + //Valor dos títulos em carteiras}
            PadRight('', 6, '0')                                           + //Quantidade títulos em cobrança
            PadRight('',17, '0')                                           + //Valor dos títulos em carteiras}
            PadRight('',6,  '0')                                           + //Quantidade títulos em cobrança
            PadRight('',17, '0')                                           + //Quantidade de Títulos em Carteiras
            PadRight('',31, ' ')                                           + //Uso exclusivo FEBRABAN/CNAB
            PadRight('',117,' ')                                           ; //Uso exclusivo FEBRABAN/CNAB}

   {GERAR REGISTRO TRAILER DO ARQUIVO}
   Result:= Result + #13#10 +
            IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
            '9999'                                                     + //Lote de serviço
            '9'                                                        + //Tipo do registro: Registro trailer do arquivo
            PadRight('',9,' ')                                             + //Uso exclusivo FEBRABAN/CNAB}
            '000001'                                                   + //Quantidade de lotes do arquivo (Registros P,Q,R, header e trailer do lote e do arquivo)
            IntToStrZero(fQtRegLote+4, 6)                              + //Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
            PadRight('',6,' ')                                             + //Uso exclusivo FEBRABAN/CNAB}
            PadRight('',205,' ');                                            //Uso exclusivo FEBRABAN/CNAB}

   fQtRegLote := 0;
   fValorTotalDocs := 0;
end;

procedure TACBrCaixaEconomica.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha := '9'                                 + // ID Registro
             Space(393)                          + // Uso Exclusivo CAIXA
             IntToStrZero(ARemessa.Count + 1, 6);  // Contador de Registros
   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrCaixaEconomica.LerRetorno240(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;
  Linha, rCedente, rCNPJCPF: String;
  rAgencia, rConta,rDigitoConta: String;
  MotivoLinha, CodMotivo: Integer;
  TempData: String;
  codOcorrencia : String;
begin

   if (copy(ARetorno.Strings[0],1,3) <> '104') then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],73,30));
   rAgencia := trim(Copy(ARetorno[0],53,5));

   if (Copy(ARetorno[0],164,3) = '047') then   //Valida Layout Cedente com 7 posiçoes
    begin
      rConta   := trim(Copy(ARetorno[0],59,6));
      rDigitoConta := Copy(ARetorno[0],65,1);
    end
    else
    begin
      rConta   := trim(Copy(ARetorno[0],59,5));
      rDigitoConta := Copy(ARetorno[0],64,1);
    end;

   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 158, 6), 0);

   TempData := Copy(ARetorno[1],192,2) + '/' + Copy(ARetorno[1],194,2) + '/' +
               Copy(ARetorno[1],198,2);

   if TempData <> '00/00/00' then
     ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(TempData,0, 'DD/MM/YY');

   TempData := Copy(ARetorno[1],200,2) + '/' + Copy(ARetorno[1],202,2) + '/' +
               Copy(ARetorno[1],206,2);

   if TempData <> '00/00/00' then
     ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(TempData, 0, 'DD/MM/YY');

    case StrToIntDef(Copy(ARetorno[0],18,1),0) of
       1: ACBrBanco.ACBrBoleto.Cedente.TipoInscricao:= pFisica;
       2: ACBrBanco.ACBrBoleto.Cedente.TipoInscricao:= pJuridica;
       else
          ACBrBanco.ACBrBoleto.Cedente.TipoInscricao:= pJuridica;
    end;

   rCNPJCPF := trim( Copy(ARetorno[0],19,14)) ;

   if ACBrBanco.ACBrBoleto.Cedente.TipoInscricao = pJuridica then
    begin
      rCNPJCPF := trim( Copy(ARetorno[1],19,15));
      rCNPJCPF := RightStr(rCNPJCPF,14) ;
    end
   else
    begin
      rCNPJCPF := trim( Copy(ARetorno[1],23,11));
      rCNPJCPF := RightStr(rCNPJCPF,11) ;
    end;

   ValidarDadosRetorno(rAgencia, rConta+rDigitoConta, rCNPJCPF, True);
   with ACBrBanco.ACBrBoleto do
   begin
     if LeCedenteRetorno then
     begin
        Cedente.Nome    := rCedente;
        Cedente.CNPJCPF := rCNPJCPF;
        Cedente.Agencia := rAgencia;
        Cedente.AgenciaDigito:= '0';
        Cedente.Conta   := rConta;
        Cedente.ContaDigito:= rDigitoConta;
        Cedente.CodigoCedente:= rConta+rDigitoConta;

        case StrToIntDef(Copy(ARetorno[1],18,1),0) of
           1: Cedente.TipoInscricao:= pFisica;
           2: Cedente.TipoInscricao:= pJuridica;
           else
              Cedente.TipoInscricao:= pJuridica;
        end;
     end;

     ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   Linha := '';
   Titulo := nil;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      {Segmento T - Só cria após passar pelo seguimento T depois U}
      if Copy(Linha,14,1)= 'T' then
         Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      if Assigned(Titulo) then
      with Titulo do
      begin
         {Segmento T}
         if Copy(Linha,14,1)= 'T' then
          begin
            SeuNumero                   := Trim(copy(Linha,106,25));
            NumeroDocumento             := copy(Linha,59,11);
            codOcorrencia               := copy(Linha,16,2);
            OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(codOcorrencia,0));
            case StrToIntDef(Copy(Linha,133,01),0) of
              00:Sacado.Pessoa:=pOutras;
              01:Sacado.Pessoa:=pFisica;
              02:Sacado.Pessoa:=pJuridica;
            end;
            Sacado.CNPJCPF              := copy(Linha,134,15);
	          Sacado.NomeSacado           := copy(Linha,149,40);
            //05 = Liquidação Sem Registro
            TempData := Copy(Linha,74,2) + '/' + Copy(Linha,76,2) + '/' + Copy(Linha,80,2);

            if TempData <> '00/00/00' then
              Vencimento := StringToDateTimeDef( TempData,0, 'DD/MM/YY');

            ValorDocumento       := StrToFloatDef(Copy(Linha,82,15),0)/100;
            ValorDespesaCobranca := StrToFloatDef(Copy(Linha,199,15),0)/100;
            NossoNumero          := Copy(Linha,42,15);

            if StrToIntDef(Copy(Linha,40,2),0) <> 0 then
              Carteira := ConverteModalidadeEmCodCarteira( StrToIntDef(Copy(Linha,40,2),0))
            else
              Carteira := ''; //TK2780

            if (CodOcorrencia  = '06' ) or (CodOcorrencia  = '09' ) or
               (CodOcorrencia  = '17' ) then
            begin
              CodigoLiquidacao     := Copy(Linha,214,02);
              CodigoLiquidacaoDescricao := CodigoLiquidacao_Descricao( StrToIntDef(CodigoLiquidacao,0) );
              case StrToIntDef(Copy(Linha,216,02),0) of
                01: Liquidacao.FormaPagto := 'Dinheiro';
                02: Liquidacao.FormaPagto := 'Cheque';
              else
                Liquidacao.FormaPagto := '';
              end;
            end;

            if (CodOcorrencia  = '02' ) or (CodOcorrencia  = '03' ) or
               (CodOcorrencia  = '26' ) or (CodOcorrencia  = '30' ) then
            begin
              MotivoLinha := 214;
              CodMotivo := StrToIntDef(IfThen(Copy(Linha, MotivoLinha, 2) = '00', '00', Copy(Linha, MotivoLinha, 2)), 0);

              if (CodMotivo <> 0) then
              begin
                MotivoRejeicaoComando.Add(IntToStr(CodMotivo));
                DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
              end;
            end;

            // informações do local de pagamento
            Liquidacao.Banco      := StrToIntDef(Copy(Linha,97,3), -1);
            Liquidacao.Agencia    := Copy(Linha,100,5);
            Liquidacao.Origem     := '';

            // quando a liquidação ocorre nos canais da caixa o banco vem zero
            // então acertar
            if Liquidacao.Banco = 0 then
              Liquidacao.Banco := 104;            
          end
         {Ssegmento U}
         else if Copy(Linha,14,1)= 'U' then
          begin

            if StrToIntDef(Copy(Linha,138,6),0) <> 0 then
               DataOcorrencia := StringToDateTimeDef( Copy(Linha,138,2)+'/'+
                                                      Copy(Linha,140,2)+'/'+
                                                      Copy(Linha,142,4),0, 'DD/MM/YYYY' );

            if StrToIntDef(Copy(Linha,146,6),0) <> 0 then
               DataCredito:= StringToDateTimeDef( Copy(Linha,146,2)+'/'+
                                                  Copy(Linha,148,2)+'/'+
                                                  Copy(Linha,150,4),0, 'DD/MM/YYYY' );

            ValorMoraJuros       := StrToFloatDef(Copy(Linha,18,15),0)/100;
            ValorDesconto        := StrToFloatDef(Copy(Linha,33,15),0)/100;
            ValorAbatimento      := StrToFloatDef(Copy(Linha,48,15),0)/100;
            ValorIOF             := StrToFloatDef(Copy(Linha,63,15),0)/100;
            ValorPago            := StrToFloatDef(Copy(Linha,78,15),0)/100;
            ValorRecebido        := StrToFloatDef(Copy(Linha,93,15),0)/100;
            ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,108,15),0)/100;
            ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,123,15),0)/100;
         end
        {Segmento W}
        else if Copy(Linha, 14, 1) = 'W' then
         begin
           //verifica o motivo de rejeição
           MotivoRejeicaoComando.Add(copy(Linha,29,2));
           DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                              CodOcorrenciaToTipo(
                                              StrToIntDef(copy(Linha, 16, 2), 0)),
                                              StrToInt(Copy(Linha, 29, 2))));
         end;
      end;
   end;

end;
function TACBrCaixaEconomica.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      01: Result := toRetornoSolicitacaoImpressaoTituloConfirmada;
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      04: Result := toRetornoTransferenciaCarteiraEntrada;
      05: Result := toRetornoTransferenciaCarteiraBaixa;
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      08: Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
      09: Result := toRetornoBaixado;
      12: Result := toRetornoRecebimentoInstrucaoConcederAbatimento;
      13: Result := toRetornoRecebimentoInstrucaoCancelarAbatimento;
      14: Result := toRetornoRecebimentoInstrucaoAlterarVencimento;
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      19: Result := toRetornoRecebimentoInstrucaoProtestar;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      23: Result := toRetornoEncaminhadoACartorio;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoAlteracaoUsoCedente;
      28: Result := toRetornoDebitoTarifas;
      30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
      35: Result := toRetornoConfirmacaoInclusaoBancoSacado;
      36: Result := toRetornoConfirmacaoAlteracaoBancoSacado;
      37: Result := toRetornoConfirmacaoExclusaoBancoSacado;
      38: Result := toRetornoEmissaoBloquetoBancoSacado;
      39: Result := toRetornoManutencaoSacadoRejeitada;
      40: Result := toRetornoEntradaTituloBancoSacadoRejeitada;
      41: Result := toRetornoManutencaoBancoSacadoRejeitada;
      44: Result := toRetornoBaixaOuLiquidacaoEstornada;
      45: Result := toRetornoRecebimentoInstrucaoAlterarDados;
      46: Result := toRetornoLiquidadoOnLine;
      47: Result := toRetornoEstornoLiquidacaoOnLine;
      51: Result := toRetornoTituloDDAReconhecidoPagador;
      52: Result := toRetornoTituloDDANaoReconhecidoPagador;
      53: Result := toRetornoTituloDDARecusadoCIP;
      61: Result := toRetornoConfirmacaoAlteracaoValorNominal;
      62: Result := toRetornoConfirmacaoAlteracaoValorpercentualMinimoMaximo;
    end;
  end
  else
  begin
    case CodOcorrencia of
      01: Result := toRetornoRegistroConfirmado;
      02: Result := toRetornoBaixaManualConfirmada;
      03: Result := toRetornoAbatimentoConcedido;
      04: Result := toRetornoAbatimentoCancelado;
      05: Result := toRetornoVencimentoAlterado;
      06: Result := toRetornoAlteracaoUsoCedente;
      07: Result := toRetornoPrazoProtestoAlterado;
      08: Result := toRetornoPrazoDevolucaoAlterado;
      09: Result := toRetornoDadosAlterados;
      10: Result := toRetornoAlteracaoReemissaoBloquetoConfirmada;
      11: Result := toRetornoAlteracaoOpcaoProtestoParaDevolucaoConfirmada;
      12: Result := toRetornoAlteracaoOpcaoDevolucaoParaProtestoConfirmada;
      20: Result := toRetornoTituloEmSer;
      21: Result := toRetornoLiquidado;
      22: Result := toRetornoLiquidadoEmCartorio;
      23: Result := toRetornoBaixadoPorDevolucao;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoEncaminhadoACartorio;
      27: Result := toRetornoProtestoSustado;
      28: Result := toRetornoEstornoProtesto;
      29: Result := toRetornoProtestoOuSustacaoEstornado;
      30: Result := toRetornoRecebimentoInstrucaoAlterarDados;
      31: Result := toRetornoTarifaDeManutencaoDeTitulosVencidos;
      32: Result := toRetornoOutrasTarifasAlteracao;
      33: Result := toRetornoEstornoBaixaLiquidacao;
      34: Result := toRetornoDebitoTarifas;
      99: Result := toRetornoRegistroRecusado;
    end;
  end;
end;

function TACBrCaixaEconomica.CodOcorrenciaToTipoRemessa(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRemessaBaixar;
    04: Result := toRemessaConcederAbatimento;
    05: Result := toRemessaCancelarAbatimento;
    06: Result := toRemessaAlterarVencimento;
    07: Result := toRemessaConcederDesconto;
    08: Result := toRemessaCancelarDesconto;
    09: Result := toRemessaProtestar;
    10: Result := toRemessaCancelarInstrucaoProtestoBaixa;
    11: Result := toRemessaCancelarInstrucaoProtesto;
    12: Result := toRemessaAlterarJurosMora;
    13: Result := toRemessaDispensarJuros;
    14: Result := toRemessaAlterarMulta; // Alteração de Valor/Percentual de Multa
    15: Result := toRemessaDispensarMulta;// Dispensar Cobrança de Multa
    16: Result := toRemessaAlterarDesconto;// Alteração do Valor de Desconto
    17: Result := toRemessaNaoConcederDesconto;// Não conceder Desconto
    18: Result := toRemessaAlterarValorAbatimento;// Alteração do Valor de Abatimento
    31: Result := toRemessaAlterarNomeEnderecoSacado;
    33: Result := toRemessaAlterarDadosRateioCredito;// Alteração dos Dados do Rateio de Crédito
    34: Result := toRemessaPedidoCancelamentoDadosRateioCredito;// Pedido de Cancelamento dos Dados do Rateio de Crédito
//    36: Result := toRemessa;// Inclusão no Banco de Pagadores
    37: Result := toRemessaAlterarDadosPagador;// Alteração no Banco de Pagadores
//    38: Result := toRemessa;// Exclusão no Banco de Pagadores
//    40: Result := toRemessa;// Serviços
    47: Result := toRemessaAlterarValorTitulo;// Alteração do Valor Nominal do Título
    48: Result := toRemessaAlterarValorMinimo;
//    48: Result := toRemessaAlterarValorMaximo; // '48' é "Alteração valor/percentualmínimo/máximo"

  else
    Result := toRemessaRegistrar;
  end;
end;

function TACBrCaixaEconomica.TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): string;
begin
  case TipoOcorrencia of
    toRemessaBaixar                               : Result := '02';
    toRemessaConcederAbatimento                   : Result := '04';
    toRemessaCancelarAbatimento                   : Result := '05';
    toRemessaAlterarVencimento                    : Result := '06';
    toRemessaConcederDesconto                     : Result := '07';
    toRemessaCancelarDesconto                     : Result := '08';
    toRemessaProtestar                            : Result := '09';
    toRemessaCancelarInstrucaoProtestoBaixa       : Result := '10';
    toRemessaCancelarInstrucaoProtesto            : Result := '11';
    toRemessaAlterarJurosMora                     : Result := '12';
    toRemessaDispensarJuros                       : Result := '13';
    toRemessaAlterarMulta                         : Result := '14';// Alteração de Valor/Percentual de Multa
    toRemessaDispensarMulta                       : Result := '15';// Dispensar Cobrança de Multa
    toRemessaAlterarDesconto                      : Result := '16';// Alteração do Valor de Desconto
    toRemessaNaoConcederDesconto                  : Result := '17';// Não conceder Desconto
    toRemessaAlterarValorAbatimento               : Result := '18';// Alteração do Valor de Abatimento
    toRemessaAlterarNomeEnderecoSacado            : Result := '31';
    toRemessaAlterarDadosRateioCredito            : Result := '33';// Alteração dos Dados do Rateio de Crédito
    toRemessaPedidoCancelamentoDadosRateioCredito : Result := '34';// Pedido de Cancelamento dos Dados do Rateio de Crédito
//    toRemessa                                     : Result := '36';// Inclusão no Banco de Pagadores
    toRemessaAlterarDadosPagador                  : Result := '37';// Alteração no Banco de Pagadores
//    toRemessa                                     : Result := '38';// Exclusão no Banco de Pagadores
//    toRemessa                                     : Result := '40';// Serviços
    toRemessaAlterarValorTitulo                   : Result := '47';// Alteração do Valor Nominal do Título
//    toRemessaAlterarValorMinimo,
//    toRemessaAlterarValorMaximo                   : Result := '48';// Alteração valor/percentualmínimo/máximo
  else
    Result := '01';
  end;
end;

function TACBrCaixaEconomica.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): string;
begin
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado, toRetornoRegistroRecusado,
      toRetornoInstrucaoRejeitada, toRetornoALteracaoOutrosDadosRejeitada:
    case CodMotivo of
      01: Result := '01-Código do Banco Inválido';
      02: Result := '02-Código do Registro Inválido';
      03: Result := '03-Código do Segmento Inválido';
      04: Result := '04-Código do Movimento não Permitido p/ Carteira';
      05: Result := '05-Código do Movimento Inválido';
      06: Result := '06-Tipo Número Inscrição Cedente Inválido';
      07: Result := '07-Agencia/Conta/DV Inválidos';
      08: Result := '08-Nosso Número Inválido';
      09: Result := '09-Nosso Número Duplicado';
      10: Result := '10-Carteira Inválida';
      11: Result := '11-Data de Geração Inválida';
      12: Result := '12-Tipo de Documento Inválido';
      13: Result := '13-Identif. Da Emissão do Bloqueto Inválida';
      14: Result := '14-Identif. Da Distribuição do Bloqueto Inválida';
      15: Result := '15-Características Cobrança Incompatíveis';
      16: Result := '16-Data de Vencimento Inválida';
      17: Result := '17-Data de Vencimento Anterior a Data de Emissão';
      18: Result := '18-Vencimento fora do prazo de operação';
      19: Result := '19-Título a Cargo de Bco Correspondentes c/ Vencto Inferior a XX Dias';
      20: Result := '20-Valor do Título Inválido';
      21: Result := '21-Espécie do Título Inválida';
      22: Result := '22-Espécie do Título Não Permitida para a Carteira';
      23: Result := '23-Aceite Inválido';
      24: Result := '24-Data da Emissão Inválida';
      25: Result := '25-Data da Emissão Posterior a Data de Entrada';
      26: Result := '26-Código de Juros de Mora Inválido';
      27: Result := '27-Valor/Taxa de Juros de Mora Inválido';
      28: Result := '28-Código do Desconto Inválido';
      29: Result := '29-Valor do Desconto Maior ou Igual ao Valor do Título';
      30: Result := '30-Desconto a Conceder Não Confere';
      31: Result := '31-Concessão de Desconto - Já Existe Desconto Anterior';
      32: Result := '32-Valor do IOF Inválido';
      33: Result := '33-Valor do Abatimento Inválido';
      34: Result := '34-Valor do Abatimento Maior ou Igual ao Valor do Título';
      35: Result := '35-Valor Abatimento a Conceder Não Confere';
      36: Result := '36-Concessão de Abatimento - Já Existe Abatimento Anterior';
      37: Result := '37-Código para Protesto Inválido';
      38: Result := '38-Prazo para Protesto Inválido';
      39: Result := '39-Pedido de Protesto Não Permitido para o Título';
      40: Result := '40-Título com Ordem de Protesto Emitida';
      41: Result := '41-Pedido Cancelamento/Sustação p/ Títulos sem Instrução Protesto';
      42: Result := '42-Código para Baixa/Devolução Inválido';
      43: Result := '43-Prazo para Baixa/Devolução Inválido';
      44: Result := '44-Código da Moeda Inválido';
      45: Result := '45-Nome do Sacado Não Informado';
      46: Result := '46-Tipo/Número de Inscrição do Sacado Inválidos';
      47: Result := '47-Endereço do Sacado Não Informado';
      48: Result := '48-CEP Inválido';
      49: Result := '49-CEP Sem Praça de Cobrança (Não Localizado)';
      50: Result := '50-CEP Referente a um Banco Correspondente';
      51: Result := '51-CEP incompatível com a Unidade da Federação';
      52: Result := '52-Unidade da Federação Inválida';
      53: Result := '53-Tipo/Número de Inscrição do Sacador/Avalista Inválidos';
      54: Result := '54-Sacador/Avalista Não Informado';
      55: Result := '55-Nosso número no Banco Correspondente Não Informado';
      56: Result := '56-Código do Banco Correspondente Não Informado';
      57: Result := '57-Código da Multa Inválido';
      58: Result := '58-Data da Multa Inválida';
      59: Result := '59-Valor/Percentual da Multa Inválido';
      60: Result := '60-Movimento para Título Não Cadastrado. Erro genérico para as situações:' + #13#10
                      + '"Cedente não cadastrado" ou' + #13#10
                      + '"Agência Cedente não cadastrada ou desativada"';
      61: Result := '61-Alteração da Agência Cobradora/DV Inválida';
      62: Result := '62-Tipo de Impressão Inválido';
      63: Result := '63-Entrada para Título já Cadastrado';
      64: Result := '64-Entrada Inválida para Cobrança Caucionada';
      65: Result := '65-CEP do Sacado não encontrado';
      66: Result := '66-Agencia Cobradora não encontrada';
      67: Result := '67-Agencia Cedente não encontrada';
      68: Result := '68-Movimentação inválida para título';
      69: Result := '69-Alteração de dados inválida';
      70: Result := '70-Apelido do cliente não cadastrado';
      71: Result := '71-Erro na composição do arquivo';
      72: Result := '72-Lote de serviço inválido';
      73: Result := '73-Código do Cedente inválido';
      74: Result := '74-Cedente não pertencente a Cobrança Eletrônica';
      75: Result := '75-Nome da Empresa inválido';
      76: Result := '76-Nome do Banco inválido';
      77: Result := '77-Código da Remessa inválido';
      78: Result := '78-Data/Hora Geração do arquivo inválida';
      79: Result := '79-Número Sequencial do arquivo inválido';
      80: Result := '80-Versão do Lay out do arquivo inválido';
      81: Result := '81-Literal REMESSA-TESTE - Válido só p/ fase testes';
      82: Result := '82-Literal REMESSA-TESTE - Obrigatório p/ fase testes';
      83: Result := '83-Tp Número Inscrição Empresa inválido';
      84: Result := '84-Tipo de Operação inválido';
      85: Result := '85-Tipo de serviço inválido';
      86: Result := '86-Forma de lançamento inválido';
      87: Result := '87-Número da remessa inválido';
      88: Result := '88-Número da remessa menor/igual remessa anterior';
      89: Result := '89-Lote de serviço divergente';
      90: Result := '90-Número sequencial do registro inválido';
      91: Result := '91-Erro seq de segmento do registro detalhe';
      92: Result := '92-Cod movto divergente entre grupo de segm';
      93: Result := '93-Qtde registros no lote inválido';
      94: Result := '94-Qtde registros no lote divergente';
      95: Result := '95-Qtde lotes no arquivo inválido';
      96: Result := '96-Qtde lotes no arquivo divergente';
      97: Result := '97-Qtde registros no arquivo inválido';
      98: Result := '98-Qtde registros no arquivo divergente';
      99: Result := '99-Código de DDD inválido';
    else
      Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end;
     toRetornoLiquidado, toRetornoBaixado:
    case CodMotivo of
      02: Result := '02-Casa Lotérica';
      03: Result := '03-Agências CAIXA';
      04: Result := '04-Compensação Eletrônica';
      05: Result := '05-Compensação Convencional';
      06: Result := '06-Internet Banking';
      07: Result := '07-Correspondente Bancário';
      08: Result := '08-Em Cartório';
      09: Result := '09-Comandada Banco';
      10: Result := '10-Comandada Cliente via Arquivo';
      11: Result := '11-Comandada Cliente On-line';
      12: Result := '12-Decurso Prazo - Cliente';
      13: Result := '13-Decurso Prazo - Banco';
      14: Result := '14-Protestado';
    else
      Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end;
    toRetornoDebitoTarifas:
    case CodMotivo of
      01: Result := '01-Tarifa de Extrato de Posição';
      02: Result := '02-Tarifa de Manutenção de Título Vencido';
      03: Result := '03-Tarifa de Sustação';
      04: Result := '04-Tarifa de Protesto';
      05: Result := '05-Tarifa de Outras Instruções';
      06: Result := '06-Tarifa de Outras Ocorrências';
      07: Result := '07-Tarifa de Envio de Duplicata ao Sacado';
      08: Result := '08-Custas de Protesto';
      09: Result := '09-Custas de Sustação de Protesto';
      10: Result := '10-Custas de Cartório Distribuidor';
      11: Result := '11-Custas de Edital';
      12: Result := '12-Redisponibilização de Arquivo Retorno Eletrônico';
      13: Result := '13-Tarifa Sobre Registro Cobrada na Baixa/Liquidação';
      14: Result := '14-Tarifa Sobre Reapresentação Automática';
      15: Result := '15-Banco de Sacados';
      16: Result := '16-Tarifa Sobre Informações Via Fax';
      17: Result := '17-Entrega Aviso Disp Bloqueto via e-amail ao sacado (s/ emissão Bloqueto)';
      18: Result := '18-Emissão de Bloqueto Pré-impresso CAIXA matricial';
      19: Result := '19-Emissão de Bloqueto Pré-impresso CAIXA A4';
      20: Result := '20-Emissão de Bloqueto Padrão CAIXA';
      21: Result := '21-Emissão de Bloqueto/Carnê';
      31: Result := '31-Emissão de Aviso de Vencido';
      42: Result := '42-Alteração cadastral de dados do título - sem emissão de aviso';
      45: Result := '45-Emissão de 2ª via de Bloqueto Cobrança Registrada';
    else
      Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end;
    end;
  end
  else
  begin
  case TipoOcorrencia of
    toRetornoTarifaDeManutencaoDeTitulosVencidos:
    case CodMotivo of
      01: Result := '01-Entrada de Título';
      02: Result := '02-Pedido de Baixa';
      03: Result := '03-Concessão de Abatimento';
      04: Result := '04-Cancelamento de Abatimento';
      05: Result := '05-Alteração de Vencimento';
      06: Result := '06-Alteração do uso da Empresa';
      07: Result := '07-Alteração do Prazo de Protesto';
      08: Result := '08-Alteração do Prazo de Devolução';
      09: Result := '09-Alteração de outros dados';
      10: Result := '10-Alt de dados c/ emissão / emissão de bloqueto';
      11: Result := '11-Alteração da opção de Protesto para Devolução';
      12: Result := '12-Alteração da opção de Devolução para Protesto';
    end;
    toRetornoRegistroRecusado:
    case CodMotivo of
      01: Result := 'Remessa sem registro tipo 0';
      02: Result := 'Identificação inválida da Empresa na CAIXA';
      03: Result := 'Número Inválido da Remessa';
      04: Result := 'Beneficiário não pertence a Cobrança Eletrônica';
      05: Result := 'Código da Remessa Inválido';
      06: Result := 'Literal da Remessa Inválido';
      07: Result := 'Código de Serviço Inválido';
      08: Result := 'Literal de Serviço Inválido';
      09: Result := 'Código do Banco Inválido';
      10: Result := 'Nome do Banco Inválido';
      11: Result := 'Data de gravação Inválida';
      12: Result := 'Número de Remessa já Processada';
      13: Result := 'Tipo de registro esperado Inválido';
      14: Result := 'Tipo de Ocorrência Inválido';
      15: Result := 'Literal Remessa Inválida para fase de Testes';
      16: Result := 'Identificação da empresa no Registro tipo 0 difere da identificação no Registro Tipo 1';
      17: Result := 'Identificação na CAIXA inválida (Nosso Número)';
      18: Result := 'Código da Carteira inválido';
      19: Result := 'Número seqüencial do Registro Inválido';
      20: Result := 'Tipo de Inscrição da empresa Inválido';
      21: Result := 'Número de Inscrição da empresa Inválido';
      22: Result := 'Literal REM.TST válida somente para a fase de Testes';
      23: Result := 'Taxa de Comissão de Permanência Inválida';
      24: Result := 'Nosso Número inválido para Cobrança Registrada emissão Beneficiário (14)';
      25: Result := 'Dígito do Nosso Número não confere';
      26: Result := 'Data de vencimento inválida';
      27: Result := 'Valor do título inválido';
      28: Result := 'Espécie de título Inválida';
      29: Result := 'Código de Aceite Inválido';
      30: Result := 'Data de emissão do título inválida';
      31: Result := 'Instrução de Cobrança 1 Inválida';
      32: Result := 'Instrução de Cobrança 2 Inválida';
      33: Result := 'Instrução de Cobrança 3 Inválida';
      34: Result := 'Valor de Juros Inválido';
      35: Result := 'Data do Desconto Inválida';
      36: Result := 'Valor do Desconto Inválido';
      37: Result := 'Valor do IOF Inválido';
      38: Result := 'Valor do Abatimento Inválido';
      39: Result := 'Tipo de Inscrição do Pagador Inválido';
      40: Result := 'Número de Inscrição do Pagador Inválido';
      41: Result := 'Número de Inscrição do Pagador obrigatório';
      42: Result := 'Nome do Pagador obrigatório';
      43: Result := 'Endereço do Pagador obrigatório';
      44: Result := 'CEP do Pagador Inválido';
      45: Result := 'Cidade do Pagador obrigatório';
      46: Result := 'Estado do Pagador obrigatório';
      47: Result := 'Data da multa inválida';
      48: Result := 'Valor da multa inválido';
      49: Result := 'Prazo de protesto/devolução inválido';
      50: Result := 'Prazo do protesto inválido';
      51: Result := 'Prazo de devolução inválido';
      52: Result := 'Moeda inválida';
      53: Result := '“USO DA EMPRESA” obrigatório';
      54: Result := 'Remessa sem registro tipo 9';
      55: Result := 'Solicitacao nao permitida para titulo incluido somente para protesto';
      56: Result := 'Identificacao inválida da empresa na CAIXA';
      57: Result := 'Identificacao inválida da empresa na CAIXA';
      58: Result := 'Identificacao inválida da empresa na CAIXA';
      59: Result := 'Identificacao inválida da empresa na CAIXA';
      60: Result := 'Identificação da emissão do bloqueto inválida';
      61: Result := 'Tipo de entrega inválido';
      62: Result := 'Modalidade do titulo inválida';
      63: Result := 'Forma de entrega de bloq.inválida para emis. banco';
      64: Result := 'Forma de entrega de bloq.inválida para emis.cedente';
      65: Result := 'Forma de emissao de bloqueto inválida';
      66: Result := 'E-mail inválido';
      67: Result := 'Número do DDD do celular do sacado inválido';
      68: Result := 'Número do celular do sacado inválido';
      69: Result := 'Tipo de mensagem de envio SMS inválido';
      70: Result := 'Envio de sms do cedente inválido';
      71: Result := 'Reenvio de SMS diferente de SMS ou SMS e postagem inválido';
    end;
    toRetornoLiquidado, toRetornoBaixado:
    case CodMotivo of
      02: Result := '02-Unidade Lotérica';
      03: Result := '03-Agências CAIXA';
      04: Result := '04-Compensação Eletrônica';
      06: Result := '06-Internet Banking';
      07: Result := '07-Correspondente CAIXAqui';
      08: Result := '08-Em Cartório';
      09: Result := '09-Comandada Banco';
      10: Result := '10-Comandada Cliente via Arquivo';
      11: Result := '11-Comandada Cliente On-line';
    else
      Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end;
  end;  
  end;

  Result := ACBrSTr(Result);
end;

function TACBrCaixaEconomica.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      01: Result := '01-Solicitação de Impressão de Títulos Confirmada';
      02: Result := '02-Entrada Confirmada';
      03: Result := '03-Entrada Rejeitada';
      04: Result := '04-Transferência de Carteira/Entrada';
      05: Result := '05-Transferência de Carteira/Baixa';
      06: Result := '06-Liquidação';
      07: Result := '07-Confirmação do Recebimento da Instrução de Desconto';
      08: Result := '08-Confirmação do Recebimento do Cancelamento do Desconto';
      09: Result := '09-Baixa';
      12: Result := '12-Confirmação Recebimento Instrução de Abatimento';
      13: Result := '13-Confirmação Recebimento Instrução de Cancelamento Abatimento';
      14: Result := '14-Confirmação Recebimento Instrução Alteração de Vencimento';
      19: Result := '19-Confirmação Recebimento Instrução de Protesto';
      20: Result := '20-Confirmação Recebimento Instrução de Sustação/Cancelamento de Protesto';
      23: Result := '23-Remessa a Cartório';
      24: Result := '24-Retirada de Cartório';
      25: Result := '25-Protestado e Baixado (Baixa por ter sido protestado)';
      26: Result := '26-Instrução Rejeitada';
      27: Result := '27-Confirmação do Pedido de Alteração de Outros Dados';
      28: Result := '28-Débito de Tarifas/Custas';
      30: Result := '30-Alteração de Dados Rejeitada';
      35: Result := '35-Confirmação de Inclusão Banco de Sacado';
      36: Result := '36-Confirmação de Alteração Banco de Sacado';
      37: Result := '37-Confirmação de Exclusão Banco de Sacado';
      38: Result := '38-Emissão de Bloquetos de Banco de Sacado';
      39: Result := '39-Manutenção de Sacado Rejeitada';
      40: Result := '40-Entrada de Título via Banco de Sacado Rejeitada';
      41: Result := '41-Manutenção de Banco de Sacado Rejeitada';
      44: Result := '44-Estorno de Baixa / Liquidação';
      45: Result := '45-Alteração de Dados';
      46: Result := '46-Liquidação On-line';
      47: Result := '47-Estorno de Liquidação On-line';
      51: Result := '51-Título DDA Reconhecido pelo Sacado';
      52: Result := '52-Título DDA Não Reconhecido pelo Sacado';
      53: Result := '53-Título DDA Recusado pela CIP';
      61: Result := '61-Confirmação de Alteração do Valor Nominal do Título';
      62: Result := '62-Confirmação de Alteração do Valor/Percentual Mínimo/Máximo';
    end;
  end
  else
  begin
    case CodOcorrencia of
      01: Result := '01-Entrada Confirmada';
      02: Result := '02-Baixa Manual Confirmada';
      03: Result := '03-Abatimento Concedido';
      04: Result := '04-Abatimento Cancelado';
      05: Result := '05-Vencimento Alterado';
      06: Result := '06-Uso da Empresa Alterado';
      07: Result := '07-Prazo de Protesto Alterado';
      08: Result := '08-Prazo de Devolução Alterado';
      09: Result := '09-Alteração Confirmada';
      10: Result := '10-Alteração com Reemissão de Bloqueto Confirmada';
      11: Result := '11-Alteração da Opção de Protesto para Devolução Confirmada';
      12: Result := '12-Alteração da Opção de Devolução para Protesto Confirmada';
      20: Result := '20-Em Ser';
      21: Result := '21-Liquidação';
      22: Result := '22-Liquidação em Cartório';
      23: Result := '23-Baixa por Devolução';
      25: Result := '25-Baixa por Protesto';
      26: Result := '26-Título Enviado para Cartório';
      27: Result := '27-Sustação de Protesto';
      28: Result := '28-Estorno de Protesto';
      29: Result := '29-Estorno de Sustação de Protesto';
      30: Result := '30-Alteração de Título';
      31: Result := '31-Tarifa sobre Título Vencido';
      32: Result := '32-Outras Tarifas de Alteração';
      33: Result := '33-Estorno de Baixa / Liquidação';
      34: Result := '34-Tarifas Diversas';
      99: Result := '99-Rejeição do Título';
    end;
  end;

  Result := ACBrSTr(Result);
end;

function TACBrCaixaEconomica.CodigoLiquidacao_Descricao(CodLiquidacao: Integer): String;
begin
  case CodLiquidacao of
    02 : result := 'Casa Lotérica';
    03 : result := 'Agências CAIXA';
    04 : result := 'Compensação Eletrônica';
    05 : result := 'Compensação Convencional';
    06 : result := 'Internet Banking';
    07 : result := 'Correspondente Bancário';
    08 : result := 'Em Cartório';
    09 : Result := 'Comandada Banco';
    10 : result := 'Comandada Cliente via Arquivo';
    11 : result := 'Comandada Cliente On-line';
    12 : result := 'Decurso Prazo - Cliente';
    13 : result := 'Decurso Prazo - Banco';
    14 : result := 'Protestado';
  end;

  Result := ACBrSTr(Result);
end;

procedure TACBrCaixaEconomica.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha : Integer;
  rAgencia, rCodCedente, Linha, rCedente , TempData:String;  
  MotivoLinha, CodMotivo: Integer;
begin
   fpTamanhoMaximoNossoNum := 15;
 
   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],47,30));
   rAgencia := Copy(ARetorno[0],27,4);
   rCodCedente := trim(Copy(ARetorno[0],31,7));


   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],390,5),0);

   TempData := Copy(ARetorno[0],95,2) + '/' + Copy(ARetorno[0],97,2) + '/' +
               Copy(ARetorno[0],99,2);

   if TempData <> '00/00/00' then
     ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(TempData, 0, 'DD/MM/YY');

   ValidarDadosRetorno(rAgencia, rCodCedente, '', True);
   with ACBrBanco.ACBrBoleto do
   begin
      if LeCedenteRetorno then
      begin
        Cedente.Nome         := rCedente;
        Cedente.Agencia      := rAgencia;
        Cedente.CodigoCedente := rCodCedente;
      end;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 15;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
     Linha := ARetorno[ContLinha] ;

     if (Copy(Linha,1,1) <> '7') and (Copy(Linha,1,1) <> '1') then
       Continue;

     Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

     with Titulo do
     begin
       SeuNumero                   := copy(Linha,32,25);
       NumeroDocumento             := copy(Linha,117,10);
       OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(
                                        copy(Linha,109,2),0));

       TempData := Copy(Linha,111,2) + '/' + Copy(Linha,113,2) + '/' +
                   Copy(Linha,115,2);

       if TempData <> '00/00/00' then
         DataOcorrencia := StringToDateTimeDef(TempData, 0, 'DD/MM/YY');

       TempData := Copy(Linha,147,2) + '/'+ Copy(Linha,149,2) + '/' +
                   Copy(Linha,151,2);

       if TempData <> '00/00/00' then
         Vencimento := StringToDateTimeDef(TempData, 0, 'DD/MM/YY');

       ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;
       ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
       ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
       ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
       ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
       ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
       ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
       if StrToIntDef(Copy(Linha,57,2),0) <> 0 then
         Carteira := ConverteModalidadeEmCodCarteira( StrToIntDef(Copy(Linha,57,2),0))
       else
         Carteira := ''; //TK2844
       NossoNumero          := Copy(Linha,59,15);
       ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;

       TempData := Copy(Linha,294,2) + '/' + Copy(Linha,296,2) + '/' +
                   Copy(Linha,298,2);

       if TempData <> '00/00/00' then
         DataCredito:= StringToDateTimeDef(TempData, 0, 'DD/MM/YY');
       case OcorrenciaOriginal.Tipo of
         toRetornoRegistroRecusado:
           begin
              MotivoLinha := 80;
              CodMotivo := StrToIntDef(IfThen(Copy(Linha, MotivoLinha, 2) = '00', '00', Copy(Linha, MotivoLinha, 2)), 0);

              if (CodMotivo <> 0) then
              begin
                MotivoRejeicaoComando.Add(IntToStr(CodMotivo));
                DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
              end;
           end;
       else
         //Não teve ocorrência...
         //MotivoLinha := 0;
         //CodMotivo := 0;
       end;
     end;
   end;

   fpTamanhoMaximoNossoNum := 15;
end;

end.
