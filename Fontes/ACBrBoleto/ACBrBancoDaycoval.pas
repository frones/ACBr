{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Juliana Rodrigues Prado                       }
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
{$I ACBr.inc}

unit ACBrBancoDaycoval;

interface

uses
  Classes, SysUtils, Contnrs, ACBrBoleto;

type
  { TACBrBancoDaycoval }

  TACBrBancoDaycoval = class(TACBrBancoClass)
  protected
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override ;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero ( const ACBrTitulo: TACBrTitulo) : String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;

    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa : TStringList);  override;

    Procedure LerRetorno400(ARetorno:TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
   end;

implementation

uses
  {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, Variants,
  ACBrValidador, ACBrUtil;

{ TACBrBancoDaycoval }

function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroRecusado :
    begin
      if ( CodMotivo = '03' ) then Result := '03-CEP inválido – Não temos cobrador – Cobrador não Localizado'
      else
      if ( CodMotivo = '04' ) then Result := '04-Sigla do Estado inválida'
      else
      if ( CodMotivo = '05' ) then Result := '05-Data de Vencimento inválida ou fora do prazo mínimo'
      else
      if ( CodMotivo = '06' ) then Result := '06-Código do Banco inválido'
      else
      if ( CodMotivo = '08' ) then Result := '08-Nome do sacado não informado'
      else
      if ( CodMotivo = '10' ) then Result := '10-Logradouro não informado'
      else
      if ( CodMotivo = '14' ) then Result := '14-Registro em duplicidade'
      else
      if ( CodMotivo = '19' ) then Result := '19-Data de desconto inválida ou maior que a data de vencimento'
      else
      if ( CodMotivo = '20' ) then Result := '20-Valor de IOF não numérico'
      else
      if ( CodMotivo = '22' ) then Result := '22-Valor de desconto + abatimento maior que o valor do título'
      else
      if ( CodMotivo = '25' ) then Result := '25-CNPJ ou CPF do sacado inválido (aceito com restrições)'
      else
      if ( CodMotivo = '26' ) then Result := '26-Espécies de documento inválida (difere de 01...10,13 e 99)'
      else
      if ( CodMotivo = '27' ) then Result := '27-Data de emissão do título inválida'
      else
      if ( CodMotivo = '28' ) then Result := '28-Seu número não informado'
      else
      if ( CodMotivo = '29' ) then Result := '29-CEP é igual a espaço ou zeros; ou não numérico'
      else
      if ( CodMotivo = '30' ) then Result := '30-Valor do título não numérico ou inválido'
      else
      if ( CodMotivo = '36' ) then Result := '36-Valor de permanência não numérico'
      else
      if ( CodMotivo = '15' ) then Result := '15-37'
      else
      if ( CodMotivo = 'Va' ) then Result := 'Valor -or de permanência inconsistente, pois, dentro de um mês, será maior que o valor do título'
      else
      if ( CodMotivo = '38' ) then Result := '38-Valor de desconto/abatimento não numérico ou inválido'
      else
      if ( CodMotivo = '39' ) then Result := '39-Valor de abatimento não numérico'
      else
      if ( CodMotivo = '42' ) then Result := '42-Título já existente em nossos registros. Nosso número não aceito'
      else
      if ( CodMotivo = '43' ) then Result := '43-Título enviado em duplicidade nesse movimento'
      else
      if ( CodMotivo = '44' ) then Result := '44-Título zerado ou em branco; ou não numérico na remessa'
      else
      if ( CodMotivo = '46' ) then Result := '46-Título enviado fora da faixa de Nosso Número, estipulada para o cliente.'
      else
      if ( CodMotivo = '51' ) then Result := '51-Tipo/Número de Inscrição Sacador/Avalista Inválido'
      else
      if ( CodMotivo = '53' ) then Result := '53-Prazo de vencimento do título excede ao da contratação'
      else
      if ( CodMotivo = '54' ) then Result := '54-Banco informado não é nosso correspondente 140-142'
      else
      if ( CodMotivo = '55' ) then Result := '55-Banco correspondente informado não cobra este CEP ou não possui faixas de CEP cadastradas'
      else
      if ( CodMotivo = '56' ) then Result := '56-Nosso número no correspondente não foi informado'
      else
      if ( CodMotivo = '57' ) then Result := '57-Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido.'
      else
      if ( CodMotivo = '58' ) then Result := '58-Entradas Rejeitadas – Reprovado no Represamento para Análise'
      else
      if ( CodMotivo = '60' ) then Result := '60-CNPJ/CPF do sacado inválido – título recusado'
      else
      if ( CodMotivo = '87' ) then Result := '87-Excede Prazo máximo entre emissão e vencimento'
      else
      if ( CodMotivo = '99' ) then Result := '99-Título não acatado pelo banco – entrar em contato Gerente da conta'
      else
      if ( CodMotivo = 'AE' ) then Result := 'AE-Título não possui abatimento'
      else
      if ( CodMotivo = 'AG' ) then Result := 'AG-Movimento não permitido – Título à vista ou contra apresentação'
      else
      if ( CodMotivo = 'AK' ) then Result := 'AK-Título pertence a outro cliente'
      else
      if ( CodMotivo = 'AL' ) then Result := 'AL-Sacado impedido de entrar nesta cobrança'
      else
      if ( CodMotivo = 'AY' ) then Result := 'AY-Título deve estar em aberto e vencido para acatar protesto'
      else
      if ( CodMotivo = 'BC' ) then Result := 'BC-Análise gerencial-sacado inválido p/operação crédito'
      else
      if ( CodMotivo = 'BD' ) then Result := 'BD-Análise gerencial-sacado inadimplente'
      else
      if ( CodMotivo = 'BE' ) then Result := 'BE-Análise gerencial-sacado difere do exigido'
      else
      if ( CodMotivo = 'BF' ) then Result := 'BF-Análise gerencial-vencto excede vencto da operação de crédito'
      else
      if ( CodMotivo = 'BG' ) then Result := 'BG-Análise gerencial-sacado com baixa liquidez'
      else
      if ( CodMotivo = 'BH' ) then Result := 'BH-Análise gerencial-sacado excede concentração'
      else
      if ( CodMotivo = 'CB' ) then Result := 'CB-Título possui protesto efetivado/a efetivar hoje'
      else
      if ( CodMotivo = 'CC' ) then Result := 'CC-Valor de iof incompatível com a espécie documento'
      else
      if ( CodMotivo = 'CD' ) then Result := 'CD-Efetivação de protesto sem agenda válida'
      else
      if ( CodMotivo = 'CE' ) then Result := 'CE-Título não aceito - pessoa física'
      else
      if ( CodMotivo = 'CF' ) then Result := 'CF-Excede prazo máximo da entrada ao vencimento'
      else
      if ( CodMotivo = 'CG' ) then Result := 'CG-Título não aceito – por análise gerencial'
      else
      if ( CodMotivo = 'CH' ) then Result := 'CH-Título em espera – em análise pelo banco'
      else
      if ( CodMotivo = 'CJ' ) then Result := 'CJ-Análise gerencial-vencto do titulo abaixo przcurto'
      else
      if ( CodMotivo = 'CK' ) then Result := 'CK-Análise gerencial-vencto do titulo abaixo przlongo'
      else
      if ( CodMotivo = 'CS' ) then Result := 'CS-Título rejeitado pela checagem de duplicatas'
      else
      if ( CodMotivo = 'CT' ) then Result := 'CT-Título já baixado'
      else
      if ( CodMotivo = 'DA' ) then Result := 'DA-Análise gerencial – Entrada de Título Descontado com limite cancelado'
      else
      if ( CodMotivo = 'DB' ) then Result := 'DB-Análise gerencial – Entrada de Título Descontado com limite vencido'
      else
      if ( CodMotivo = 'DC' ) then Result := 'DC-Análise gerencial - cedente com limite cancelado'
      else
      if ( CodMotivo = 'DD' ) then Result := 'DD-Análise gerencial – cedente é sacado e teve seu limite cancelado'
      else
      if ( CodMotivo = 'DE' ) then Result := 'DE-Análise gerencial - apontamento no Serasa'
      else
      if ( CodMotivo = 'DG' ) then Result := 'DG-Endereço sacador/avalista não informado'
      else
      if ( CodMotivo = 'DH' ) then Result := 'DH-Cep do sacador/avalista não informado'
      else
      if ( CodMotivo = 'DI' ) then Result := 'DI-Cidade do sacador/avalista não informado'
      else
      if ( CodMotivo = 'DJ' ) then Result := 'DJ-Estado do sacador/avalista inválido ou n informado'
      else
      if ( CodMotivo = 'DM' ) then Result := 'DM-Cliente sem Código de Flash cadastrado no cobrador'
      else
      if ( CodMotivo = 'DN' ) then Result := 'DN-Título Descontado com Prazo ZERO – Recusado'
      else
      if ( CodMotivo = 'DO' ) then Result := 'DO-Título em Prejuízo'
      else
      if ( CodMotivo = 'DP' ) then Result := 'DP-Data de Referência menor que a Data de Emissão do Título'
      else
      if ( CodMotivo = 'DT' ) then Result := 'DT-Nosso Número do Correspondente não deve ser informado'
      else
      if ( CodMotivo = 'EB' ) then Result := 'EB-HSBC não aceita endereço de sacado com mais de 38 caracteres'
      else
        Result := CodMotivo + '-Motivo desconhecido';
    end;
    toRetornoBaixaRejeitada :
    begin
      if ( CodMotivo = '05' ) then Result := '05-Solicitação de baixa para título já baixado ou liquidado'
      else
      if ( CodMotivo = '06' ) then Result := '06-Solicitação de baixa para título não registrado no sistema'
      else
      if ( CodMotivo = '08' ) then Result := '08-Solicitação de baixa para título em float'
      else
        Result := CodMotivo + '-Motivo desconhecido';
    end;
    toRetornoInstrucaoRejeitada :
    begin
      if ( CodMotivo = '04' ) then Result := '04-Data de Vencimento não numérica ou inválida'
      else
      if ( CodMotivo = '14' ) then Result := '14-Registro em duplicidade'
      else
      if ( CodMotivo = '20' ) then Result := '20-Campo livre informado'
      else
      if ( CodMotivo = '21' ) then Result := '21-Título não registrado no sistema'
      else
      if ( CodMotivo = '22' ) then Result := '22-Título baixada ou liquidado'
      else
      if ( CodMotivo = '27' ) then Result := '27-Instrução não aceita, pôr não ter sido emitida ordem de protesto ao cartório'
      else
      if ( CodMotivo = '28' ) then Result := '28-Título tem instrução de cartório ativa'
      else
      if ( CodMotivo = '29' ) then Result := '29-Título não tem instrução de cartório ativa'
      else
      if ( CodMotivo = '30' ) then Result := '30-Existe instrução de não protestar, ativa para o título'
      else
      if ( CodMotivo = '37' ) then Result := '37-Título Descontado Instrução não permitida para a carteira'
      else
      if ( CodMotivo = '38' ) then Result := '38-Valor do abatimento não numérico ou maior que a soma do valor do título + permanência + multa'
      else
      if ( CodMotivo = '49' ) then Result := '49-Título em cartório'
      else
      if ( CodMotivo = '40' ) then Result := '40-Instrução recusada - cobrança vinculada / caucionada'
      else
      if ( CodMotivo = '99' ) then Result := '99-Ocorrência desconhecida na remessa'
      else
        Result := CodMotivo + '-Motivo desconhecido';
    end;
  end;
end;

constructor TACBrBancoDaycoval.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 2;
   fpNome                  := 'Banco Daycoval';
   fpNumero                := 707;
   fpTamanhoMaximoNossoNum := 10;
   fpTamanhoAgencia        := 4;
   fpTamanhoConta          := 7;
   fpTamanhoCarteira       := 3;
end;

function TACBrBancoDaycoval.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
var
  Docto: String;
begin
   Result := '0';
   Docto := '';

   with ACBrTitulo do
   begin
      if MatchText( Carteira , ['116','117','119','134','135','136','104',
      '147','105','112','212','166','113','126','131','145','150','168']) then
            Docto := Carteira + PadLeft(NossoNumero,TamanhoMaximoNossoNum,'0')
         else
            Docto := ACBrBoleto.Cedente.Agencia + ACBrBoleto.Cedente.Conta +
                     Carteira + PadLeft(ACBrTitulo.NossoNumero,TamanhoMaximoNossoNum,'0')
   end;

   Modulo.MultiplicadorInicial := 1;
   Modulo.MultiplicadorFinal   := 2;
   Modulo.MultiplicadorAtual   := 2;
   Modulo.FormulaDigito := frModulo10;
   Modulo.Documento:= Docto;
   Modulo.Calcular;
   Result := IntToStr(Modulo.DigitoFinal);

end;

function TACBrBancoDaycoval.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras :String;
  ANossoNumero, aAgenciaCC : string;
begin
  {Codigo de Barras}
  with ACBrTitulo.ACBrBoleto do
  begin
     FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

     ANossoNumero := PadLeft(ACBrTitulo.NossoNumero,10,'0') +
                     CalcularDigitoVerificador(ACBrTitulo);

     aAgenciaCC   := Cedente.Agencia +
                     Cedente.Conta   +
                     Cedente.ContaDigito;

     aAgenciaCC:= OnlyNumber(aAgenciaCC);

     CodigoBarras := IntToStr( Numero ) +
                     '9' +
                     FatorVencimento +
                     IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                     Cedente.Agencia +
                     ACBrTitulo.Carteira +
                     Cedente.Operacao +
                     ANossoNumero;

     DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
  end;

  Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 39) ;
end;

function TACBrBancoDaycoval.MontarCampoNossoNumero ( const ACBrTitulo: TACBrTitulo
   ) : String;
var
  NossoNr: String;
begin
  with ACBrTitulo do
  begin
    NossoNr := Carteira + PadLeft(NossoNumero,TamanhoMaximoNossoNum,'0');
  end;

  Insert('/',NossoNr,4);  Insert('-',NossoNr,15);
  Result := NossoNr + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoDaycoval.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '-' + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito +'/'+
             ACBrTitulo.ACBrBoleto.Cedente.Conta    +'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

procedure TACBrBancoDaycoval.GerarRegistroHeader400(
  NumeroRemessa: Integer; aRemessa: TStringList);
var
  wLinha: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    wLinha :=
      '0' +                             // Código do registro: 0 - Header
      '1' +                             // Código do arquivo: 1 - Remessa
      'REMESSA' +                       // Identificação do arquivo
      '01' +                            // Código do serviço
      PadRight('COBRANCA',15) +         // Identificação do serviço
      PadRight(CodigoCedente, 12) +     // Código da empresa no banco
      Space(8) +                        // Brancos
      PadRight(Nome, 30) +              // Nome da empresa
      '707' +                           // Código do banco: 707 = Banco Daycoval
      PadRight('BANCO DAYCOVAL', 15) +  // Nome do banco
      FormatDateTime('ddmmyy', Now) +   // Data de gravação
      Space(294) +                      // Brancos
      IntToStrZero(1, 6);               // Número sequencial do registro

    ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
  end;
end;

procedure TACBrBancoDaycoval.GerarRegistroTransacao400( ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  ATipoOcorrencia, AEspecieDoc, ACodigoRemessa : String;
  DiasProtesto, TipoSacado, ATipoAceite: String;
  wLinha: String;
begin
  with ACBrTitulo do
  begin
    // Definindo o código da ocorrência.
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar            : ATipoOcorrencia := '02'; // Pedido de baixa
      toRemessaConcederAbatimento: ATipoOcorrencia := '04'; // Concessão de abatimento
      toRemessaCancelarAbatimento: ATipoOcorrencia := '05'; // Cancelamento de abatimento concedido
      toRemessaAlterarVencimento : ATipoOcorrencia := '06'; // Alteração de vencimento
      toRemessaAlterarUsoEmpresa : ATipoOcorrencia := '07'; // Alteração "Uso Exclusivo do Cliente"
      toRemessaAlterarSeuNumero  : ATipoOcorrencia := '08'; // Alteração de "Seu Número"
      toRemessaProtestar         : ATipoOcorrencia := '09'; // Pedido de protesto
      toRemessaNaoProtestar      : ATipoOcorrencia := '10'; // Não protestar
      toRemessaDispensarJuros    : ATipoOcorrencia := '11'; // Não cobrar juros de mora
    else
      ATipoOcorrencia := '01'; // Remessa
    end;

    // Definindo a espécie do título.
    if AnsiSameText(EspecieDoc, 'DM') then
      AEspecieDoc := '01'
    else if AnsiSameText(EspecieDoc, 'NP') then
      AEspecieDoc := '02'
    else if AnsiSameText(EspecieDoc, 'NS') then
      AEspecieDoc := '03'
    else if AnsiSameText(EspecieDoc, 'RC') then
      AEspecieDoc := '05'
    else if AnsiSameText(EspecieDoc, 'DS') then
      AEspecieDoc := '09'
    else
      AEspecieDoc := EspecieDoc;

    if (DataProtesto > 0) and (DataProtesto > Vencimento) then
      DiasProtesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento), 2)
    else
      DiasProtesto := '00';

    // Definindo o tipo de inscrição do sacado.
    case Sacado.Pessoa of
      pFisica  : TipoSacado := '01';
      pJuridica: TipoSacado := '02';
    else
      TipoSacado := '03';
    end;

    // Conforme manual o aceite deve ser sempre 'N'
    ATipoAceite := 'N';

    // Código de Remessa
    // Se o boleto for emitido pelo banco, o código da remessa deve ser 3
    // Se o boleto for emitido pelo cliente, o código da remessa deve ser 4
    if ( ACBrBoleto.Cedente.ResponEmissao = tbCliEmite ) then
      ACodigoRemessa := '4'
    else
      ACodigoRemessa := '3';

    with ACBrBoleto do
    begin
      wLinha :=
        '1' +                                                        // Código do registro: 1 - Transação
        TipoSacado +                                                 // Tipo de inscrição da empresa: 01 = CPF; 02 = CNPJ
        PadLeft(OnlyNumber(Cedente.CNPJCPF), 14, '0') +              // Número de inscrição
        PadRight(Cedente.CodigoCedente, 20) +                        // Código da empresa no banco
        PadRight(SeuNumero, 25) +                                    // Identificação do título na empresa
        PadRight(NossoNumero,8) +                                    // Nosso número
        PadLeft('',5,'0')  +                                         // Zeros
        PadRight(NossoNumero,8) +                                    // Nosso número correspondente
        Space(24) +                                                  // Brancos
        ACodigoRemessa +                                             // Código da Remessa (3-Banco Emite / 4-Cliente emite)
        ATipoOcorrencia +                                            // Código da ocorrência
        PadLeft(RightStr(SeuNumero,10), 10, ' ') +                               // Identificação do título na empresa
        FormatDateTime('ddmmyy', Vencimento) +                       // Data de vencimento do título
        IntToStrZero(Round(ValorDocumento * 100), 13) +              // Valor nominal do título
        '707' +                                                      // Banco encarregado da cobrança: 707 = Banco Daycoval
        '00000' +                                                    // Agência encarregada da cobrança + digito
        AEspecieDoc +                                                // Espécie do título
        ATipoAceite +                                                // Identificação de aceite do título: A = Aceito; N = Não aceito
        FormatDateTime('ddmmyy', DataDocumento) +                    // Data de emissão do título
        PadLeft('', 2, '0') +                                        // zeros
        PadLeft('', 2, '0') +                                        // zeros
        PadLeft('', 13, '0') +                                        // zeros
        IfThen(DataDesconto > 0,
          FormatDateTime('ddmmyy', DataDesconto), '000000') +        // Data limite para desconto
        IntToStrZero(Round(ValorDesconto * 100), 13) +               // Valor do desconto
        PadLeft('', 26, '0') +                                       // Valor do IOF (zeros segundo o manual)
        TipoSacado +                                                 // Tipo de inscrição do sacado
        PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0') +                  // Número de inscrição do sacado
        PadRight(Sacado.NomeSacado, 30, ' ') +                           // Nome do sacado
        Space(10) +                                                  // Brancos
        PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' ' + Sacado.Complemento, 40, ' ') +     // Endereço do sacado
        PadRight(Sacado.Bairro, 12, ' ') +                               // Bairro do sacado
        PadRight(OnlyNumber(Sacado.CEP), 8, '0') +                       // CEP do sacado
        PadRight(Sacado.Cidade, 15, ' ') +                               // Cidade do sacado
        PadRight(Sacado.UF, 2, ' ') +                                    // UF do sacado
        PadRight(Sacado.SacadoAvalista.NomeAvalista,30) +                  // Nome do sacador avalista
        Space(4) +                                                   // Brancos
        Space(6) +                                                   // Brancos
        PadLeft('', 2, '0') +                                        // zeros
        '0' ;                                                        // Moeda 0=Moeda nacional atual 3=Dolar

      wLinha := wLinha + IntToStrZero(ARemessa.Count + 1, 6); // Número sequencial do registro

      ARemessa.Text := ARemessa.Text + UpperCase(wLinha);
    end;
  end;
end;

procedure TACBrBancoDaycoval.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  wLinha: String;
begin
  wLinha :=
    '9' +                                // Código do registro: 9 - Trailler
    Space(393) +                         // Brancos
    IntToStrZero(ARemessa.Count + 1, 6); // Número sequencial do registro

  ARemessa.Text := ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrBancoDaycoval.LerRetorno400(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  ContLinha: Integer;
  CodMotivo: String;
  rAgencia: String;
  rConta, rDigitoConta: String;
  Linha, rCedente, rCNPJCPF: String;
  rCodEmpresa: String;
begin
  // Foi necessário utilizar o número e nome do Banco Daycoval
  fpNumero := 707;
  fpNome   := 'Banco Daycoval';

  if StrToIntDef(Copy(ARetorno.Strings[0], 77, 3), -1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
      ' não é um arquivo de retorno do ' + Nome));

  rCodEmpresa  := Trim(Copy(ARetorno[0], 27, 12));
  rCedente     := Trim(Copy(ARetorno[0], 47, 30));

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 109, 5), 0);

  ACBrBanco.ACBrBoleto.DataArquivo :=
    StringToDateTimeDef(
      Copy(ARetorno[0], 95, 2) + '/' +
      Copy(ARetorno[0], 97, 2) + '/' +
      Copy(ARetorno[0], 99, 2), 0, 'dd/mm/yy');

  case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
    1: rCNPJCPF := Copy(ARetorno[1], 7, 11);
    2: rCNPJCPF := Copy(ARetorno[1], 4, 14);
  else
    rCNPJCPF := Copy(ARetorno[1], 4, 14);
  end;

  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCodEmpresa <> PadLeft(Cedente.CodigoCedente, 12, '0')) then
      raise Exception.Create(ACBrStr('Código da Empresa do arquivo inválido.'));

    case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
      1: Cedente.TipoInscricao:= pFisica;
      2: Cedente.TipoInscricao:= pJuridica;
    else
      Cedente.TipoInscricao:= pJuridica;
    end;

    if LeCedenteRetorno then
    begin
      Cedente.CNPJCPF       := rCNPJCPF;
      Cedente.CodigoCedente := rCodEmpresa;
      Cedente.Nome          := rCedente;
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha];

    if Copy(Linha, 1, 1) <> '1' then
      Continue;

    Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      SeuNumero               := Copy(Linha, 38, 25);
      NumeroDocumento         := Copy(Linha, 117, 10);
      OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(Copy(Linha, 109, 2), 0));

      CodMotivo := Trim(Copy(Linha, 378, 2));

      if ( CodMotivo <> '' ) then
      begin
        MotivoRejeicaoComando.Add(CodMotivo);
        DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, CodMotivo));
      end;

      DataOcorrencia :=
        StringToDateTimeDef(
          Copy(Linha, 111, 2) + '/' +
          Copy(Linha, 113, 2) + '/'+
          Copy(Linha, 115, 2), 0, 'dd/mm/yy');

      if Copy(Linha, 147, 2) <> '00' then
        Vencimento :=
          StringToDateTimeDef(
            Copy(Linha, 147, 2) + '/' +
            Copy(Linha, 149, 2) + '/'+
            Copy(Linha, 151, 2), 0, 'dd/mm/yy');

      ValorDocumento       := StrToFloatDef(Copy(Linha, 153, 13), 0) / 100;
      ValorIOF             := StrToFloatDef(Copy(Linha, 215, 13), 0) / 100;
      ValorAbatimento      := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
      ValorDesconto        := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
      ValorMoraJuros       := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
      ValorRecebido        := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;
      NossoNumero          := Copy(Linha, 95, TamanhoMaximoNossoNum);
      Carteira             := Copy(Linha, 108, 1);
      ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 176, 13), 0) / 100;

    end;
  end;
end;

function TACBrBancoDaycoval.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia), 0);

  case CodOcorrencia of
    02: Result := '02 Entrada Confirmada';
    03: Result := '03 Entrada Rejeitada';
    05: Result := '05 Campo Livre Alterado';
    06: Result := '06 Liquidação Normal';
    08: Result := '08 Liquidação em Cartório';
    09: Result := '09 Baixa Automática';
    10: Result := '10 Baixa pôr ter sido liquidado';
    12: Result := '12 Confirma Abatimento';
    13: Result := '13 Abatimento Cancelado';
    14: Result := '14 Vencimento Alterado';
    15: Result := '15 Baixa Rejeitada';
    16: Result := '16 Instrução Rejeitada';
    19: Result := '19 Confirma Recebimento de Ordem de Protesto';
    20: Result := '20 Confirma Recebimento de Ordem de Sustação';
    22: Result := '22 Seu Número Alterado';
    23: Result := '23 Título enviado para Cartório';
    24: Result := '24 Confirma recebimento de ordem de não protestar';
    28: Result := '28 Débito de tarifas/custas – Correspondentes';
    40: Result := '40 Tarifa de entrada (debitada na liquidação)';
    43: Result := '43 Baixado por ter sido protestado';
    96: Result := '96 Tarifa sobre instruções – Mês anterior';
    97: Result := '97 Tarifa sobre baixas – Mês anterior';
    98: Result := '98 Tarifa sobre entradas – Mês anterior';
    99: Result := '99 Tarifa sobre instrução de protesto/sustação – mês anterior';
  else
    Result := IntToStr(CodOcorrencia)+' Ocorrência desconhecida';
  end;
end;

function TACBrBancoDaycoval.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    05: Result := toRetornoAlteracaoSeuNumero;
    06: Result := toRetornoLiquidado;
    08: Result := toRetornoLiquidadoEmCartorio;
    09: Result := toRetornoBaixaAutomatica;
    10: Result := toRetornoBaixaPorTerSidoLiquidado;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoBaixaRejeitada;
    16: Result := toRetornoInstrucaoRejeitada;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    22: Result := toRetornoAlteracaoSeuNumero;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoRecebimentoInstrucaoNaoProtestar;
    28: Result := toRetornoDebitoTarifas;
    40: Result := toRetornoDebitoTarifas;
    43: Result := toRetornoProtestado;
    96: Result := toRetornoDebitoTarifas;
    97: Result := toRetornoDebitoTarifas;
    98: Result := toRetornoDebitoTarifas;
    99: Result := toRetornoDebitoTarifas;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoDaycoval.TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                 : Result := '02';
    toRetornoRegistroRecusado                   : Result := '03';
    toRetornoAlteracaoSeuNumero                 : Result := '05';
    toRetornoLiquidado                          : Result := '06';
    toRetornoLiquidadoEmCartorio                : Result := '08';
    toRetornoBaixaAutomatica                    : Result := '09';
    toRetornoBaixaPorTerSidoLiquidado           : Result := '10';
    toRetornoAbatimentoConcedido                : Result := '12';
    toRetornoAbatimentoCancelado                : Result := '13';
    toRetornoVencimentoAlterado                 : Result := '14';
    toRetornoBaixaRejeitada                     : Result := '15';
    toRetornoInstrucaoRejeitada                 : Result := '16';
    toRetornoRecebimentoInstrucaoProtestar      : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto : Result := '20';
    toRetornoEncaminhadoACartorio               : Result := '23';
    toRetornoRecebimentoInstrucaoNaoProtestar   : Result := '24';
    toRetornoDebitoTarifas                      : Result := '28';
    toRetornoProtestado                         : Result := '43';
  else
    Result := '02';
  end;
end;

function TACBrBancoDaycoval.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarUsoEmpresa;               {Alteração do uso Da Empresa}
    08 : Result:= toRemessaAlterarSeuNumero;                {Alteração do seu Número}
    09 : Result:= toRemessaProtestar;                       {Protestar (emite aviso ao sacado após xx dias do vencimento, e envia ao cartório após 5 dias úteis)}
    10 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar Protesto}
    11 : Result:= toRemessaProtestoFinsFalimentares;        {Protesto para fins Falimentares}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    30 : Result:= toRemessaExcluirSacadorAvalista;          {Exclusão de Sacador Avalista}
    31 : Result:= toRemessaOutrasAlteracoes;                {Alteração de Outros Dados}
    34 : Result:= toRemessaBaixaporPagtoDiretoCedente;      {Baixa por ter sido pago Diretamente ao Cedente}
    35 : Result:= toRemessaCancelarInstrucao;               {Cancelamento de Instrução}
    37 : Result:= toRemessaAlterarVencimentoSustarProtesto; {Alteração do Vencimento e Sustar Protesto}
    38 : Result:= toRemessaCedenteDiscordaSacado;           {Cedente não Concorda com Alegação do Sacado }
    47 : Result:= toRemessaCedenteSolicitaDispensaJuros;    {Cedente Solicita Dispensa de Juros}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

end.
