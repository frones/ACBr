  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
  {                                                                              }
  { Colaboradores nesse arquivo: Victor H Gonzales                               }
  {                                                                              }
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

  //Incluido 25/03/2023

{$I ACBr.inc}
unit ACBrBancoFibra;

interface

uses
  Classes,
  Contnrs,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoConversao;

type

    { TACBrBancoFibra }

  TACBrBancoFibra = class(TACBrBancoClass)
  private
    procedure GerarRegistrosNFe(ACBrTitulo : TACBrTitulo; aRemessa: TStringList);
  protected
    function ConverterDigitoModuloFinal(): String; override;
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function DefineCarteira(const ACBrTitulo: TACBrTitulo): String;
    function DefineCarteiraRemessa(const ACBrTitulo: TACBrTitulo): String;
    function GetLocalPagamento: String; override;
  public
    Constructor create(AOwner: TACBrBanco);
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList); override;
    function MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo; const nRegistro: Integer): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function DefineTipoMulta(const ATitulo : TACBrTitulo):String;
    procedure EhObrigatorioAgenciaDV; override;
    procedure EhObrigatorioContaDV; override;
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;

  end;

implementation

uses
  StrUtils,
  ACBrBase,
  ACBrUtil.Strings,
  ACBrUtil.Base, ACBrValidador;

  { TACBrBancoFibra }

function TACBrBancoFibra.ConverterDigitoModuloFinal(): String;
begin
  if Modulo.ModuloFinal = 1 then
    Result := 'P'
  else
    Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoFibra.DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
begin

  Result :=
   PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4, '0') +
   PadLeft(DefineCarteira(ACBrTitulo), 3, '0') +
   PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Operacao, 7, '0') +
   PadLeft(ACBrTitulo.NossoNumero, 10, '0') +
   CalcularDigitoVerificador(ACBrTitulo);


end;

function TACBrBancoFibra.DefineCarteira(const ACBrTitulo: TACBrTitulo): String;
var LCarteira : String;
begin
  LCarteira := ACBrTitulo.Carteira;
  if LCarteira = 'D' then
    Result := '110'
  else if LCarteira = '6' then
    Result := '121'
  else
    Result := ACBrTitulo.Carteira;
end;

function TACBrBancoFibra.DefineCarteiraRemessa(const ACBrTitulo: TACBrTitulo): String;
begin
  case StrToIntDef(ACBrTitulo.Carteira,0) of
    110 : Result := 'D';
    121 : Result := '6';
    else
      Result := ACBrTitulo.Carteira;
  end;
end;

function TACBrBancoFibra.DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String;
begin
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'DM') then
    Result := '01'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'NP') then
    Result := '02'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CQ') then
    Result := '03'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'LC') then
    Result := '04'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'REC') then
    Result := '05'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'AS') then
    Result := '08'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'DS') then
    Result := '12'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CC') then
    Result := '31'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'OUT') then
    Result := '99'
  else
    Result := ACBrTitulo.EspecieDoc;
end;

function TACBrBancoFibra.DefineTipoMulta(const ATitulo: TACBrTitulo): String;
begin
  Result := '0';
  if ATitulo.PercentualMulta > 0 then
    if ATitulo.MultaValorFixo then
      Result := '2'
    else
      Result := '1';
end;

procedure TACBrBancoFibra.EhObrigatorioAgenciaDV;
begin
  //
end;

procedure TACBrBancoFibra.EhObrigatorioContaDV;
begin
  //
end;

constructor TACBrBancoFibra.create(AOwner: TACBrBanco);
begin
  inherited create(AOwner);
  fpDigito                     := 0;
  fpNome                       := 'FIBRA';
  fpNumero                     := 224;
  fpTamanhoMaximoNossoNum      := 10;
  fpTamanhoAgencia             := 4;
  fpTamanhoConta               := 9;
  fpTamanhoCarteira            := 3;
  fpLayoutVersaoArquivo        := 22;
  fpLayoutVersaoLote           := 0;
  fpDensidadeGravacao          := '';
  fpCodParametroMovimento      := '';
  fpOrientacoesBanco.Add(ACBrStr('TÍTULO CEDIDO AO BANCO FIBRA SA A SER PAGO APENAS POR ESTE BOLETO.'));
end;

function TACBrBancoFibra.MontarCampoCodigoCedente(
  const ACBrTitulo: TACBrTitulo): String;
begin
  Result := PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia,4,'0') +
    '/'+
    PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Conta,9,'0');
end;

function TACBrBancoFibra.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := 
    DefineCarteira(ACBrTitulo) +
    '/'+
    ACBrTitulo.NossoNumero +
    '-'+
    CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoFibra.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.create(ACBrStr('Não implementado CNAB240'));
end;

procedure TACBrBancoFibra.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
var
  LLinha      : String;
  Beneficiario: TACBrCedente;
begin
  Beneficiario := ACBrBanco.ACBrBoleto.Cedente;

  LLinha := '0' +                                                               // 001 a 001 - Tipo de Registro
    '1' +                                                                       // 002 a 002 - Código Remessa
    'REMESSA' +                                                                 // 003 a 009 - Literal Remessa
    '01' +                                                                      // 010 a 011 - Código Serviço
    PadRight('COBRANCA', 15) +                                                  // 012 a 026 - Literal Cobranca
    PadRight(Beneficiario.CodigoCedente, 20) +                                  // 027 a 046 - Código do Cedente
    PadRight(Beneficiario.Nome, 30) +                                           // 047 a 076 - Nome da Empresa
    IntToStrZero(Numero, 3) +                                                   // 077 a 079 - Código do Banco
    PadRight(Nome, 15) +                                                        // 080 a 094 - Uso do Banco Brancos
    FormatDateTime('ddmmyy', Now) +                                             // 095 a 100 - Data de Gravação
    Space(294) +                                                                // 101 a 394 - Uso do Banco Brancos
    IntToStrZero(1, 6);                                                         // 395 a 400 - Sequencial

  ARemessa.Add(UpperCase(LLinha));
end;

procedure TACBrBancoFibra.GerarRegistrosNFe(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  LQtdRegNFes, X, I, LQtdNFeNaLinha: Integer;
  LLinha, LNFeSemDados: String;
  LContinua: Boolean;
begin
   LNFeSemDados:= StringOfChar(' ',15) + StringOfChar('0', 65);
   LQtdRegNFes:= trunc(ACBrTitulo.ListaDadosNFe.Count / 3);

   if (ACBrTitulo.ListaDadosNFe.Count mod 3) <> 0 then
      Inc(LQtdRegNFes);

   X:= 0;
   I:= 0;
   repeat
   begin
      LContinua:=  true;

      LLinha:= '4';
      LQtdNFeNaLinha:= 0;
      while (LContinua) and (X < ACBrTitulo.ListaDadosNFe.Count) do
      begin
         LLinha:= LLinha +
                  PadRight(ACBrTitulo.ListaDadosNFe[X].NumNFe,15) +
                  IntToStrZero(round(ACBrTitulo.ListaDadosNFe[X].ValorNFe  * 100 ), 13) +
                  FormatDateTime('ddmmyyyy',ACBrTitulo.ListaDadosNFe[X].EmissaoNFe) +
                  PadLeft(ACBrTitulo.ListaDadosNFe[X].ChaveNFe, 44, '0');

         Inc(X);
         Inc(LQtdNFeNaLinha);
         LContinua := (X mod 3) <> 0 ;
      end;

      if LQtdNFeNaLinha < 3 then
      begin
         LLinha:= LLinha + LNFeSemDados;
         if LQtdNFeNaLinha < 2 then
            LLinha:= LLinha + LNFeSemDados;
      end;

      LLinha:= PadRight(LLinha,241) + StringOfChar(' ', 153) +
               IntToStrZero(aRemessa.Count + 1, 6);

      aRemessa.Add(LLinha);
      Inc(I);
   end;
   until (I = LQtdRegNFes);
end;

procedure TACBrBancoFibra.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList);
var
  LTitulo     : TACBrTitulo;
  Beneficiario: TACBrCedente;
  Pagador     : TACBrSacado;
  LLinha      : String;
begin

  LTitulo      := ACBrTitulo;
  Beneficiario := LTitulo.ACBrBoleto.Cedente;
  Pagador      := LTitulo.Sacado;

  LLinha := '1' +                                                               // 001 a 001 - Tipo de Registro
    PadLeft(DefineTipoInscricao, 2, '0') +                                      // 002 a 003 - Tipo de Inscrição Empresa
    PadLeft(OnlyNumber(Beneficiario.CNPJCPF), 14, '0') +                        // 004 a 017 - CNPJ Empresa
    PadRight(Beneficiario.CodigoCedente, 20) +                                  // 018 a 037 - Código da Empresa
    PadRight(LTitulo.NumeroDocumento, 25) +                                     // 038 a 062 - Uso da Empresa
    PadRight(LTitulo.NossoNumero+
             CalcularDigitoVerificador(ACBrTitulo), 11) +                       // 063 a 073 - Nosso Número
    Poem_Zeros('',13) +                                                         // 074 a 086 - Uso do Banco Brancos
    Space(3) +                                                                  // 087 a 089 - Uso do Banco Brancos
    DefineTipoMulta(LTitulo) +                                                  // 090 a 090 - Tipo de Multa
    PadRight(ifThen(LTitulo.PercentualMulta > 0,
       IntToStrZero(round(LTitulo.PercentualMulta *100 ),13),'0'),13,'0') +     // 091 a 103 - Valor Multa
    PadLeft(IfThen((LTitulo.DataMulta - LTitulo.Vencimento > 0),
       IntToStr(Trunc(LTitulo.DataMulta - LTitulo.Vencimento)),'0'),2,'0') +    // 104 a 105 - Numero de Dias
    Space(2) +                                                                  // 106 a 107 - Uso do Banco Brancos
    PadLeft(DefineCarteiraRemessa(LTitulo), 1, '0') +                           // 108 a 108 - Código da Carteira
       TipoOcorrenciaToCodRemessa(LTitulo.OcorrenciaOriginal.Tipo) +            // 109 a 110 - Código Ocorrência Remessa
    PadRight(LTitulo.SeuNumero, 10) +                                           // 111 a 120 - Seu Número
    FormatDateTime('ddmmyy', LTitulo.Vencimento) +                              // 121 a 126 - Data Vencimento
    IntToStrZero(round(LTitulo.ValorDocumento*100), 13) +                       // 127 a 139 - Valor Titulo
    IntToStrZero(Numero, 3) +                                                   // 140 a 142 - Código do Banco
    Poem_Zeros('', 4) +                                                         // 143 a 146 - Agencia Cobradora Zeros
    Poem_Zeros('', 1) +                                                         // 147 a 147 - DAC da Agencia Cobradora Zeros
    DefineEspecieDoc(ACBrTitulo) +                                              // 148 a 149 - Espécie do Título
    DefineAceite(LTitulo) +                                                     // 150 a 150 - Aceite
    FormatDateTime('ddmmyy', LTitulo.DataDocumento) +                           // 151 a 156 - Data Emissão Título
    InstrucoesProtesto(LTitulo) +                                               // 157 a 158 - Instrucao 1 159 a 160 - Instrucao 2
    IntToStrZero(round(LTitulo.ValorMoraJuros*100),13) +                        // 161 a 173 - Juros ao Dia
    IfThen(LTitulo.DataDesconto < EncodeDate(2000, 01, 01),
        '000000',
        FormatDateTime('ddmmyy', LTitulo.DataDesconto)) +                       // 174 a 179 - Data Desconto
    IntToStrZero(round(LTitulo.ValorDesconto*100),13) +                         // 180 a 192 - Valor Desconto
    IntToStrZero(round(LTitulo.ValorIOF*100),13) +                              // 193 a 205 - Valor IOF
    IntToStrZero(round(LTitulo.ValorAbatimento*100),13) +                       // 206 a 218 - Valor Abatimento
    DefineTipoSacado(LTitulo) +                                                 // 219 a 220 - Tipo Sacado
    PadLeft(OnlyNumber(Pagador.CNPJCPF), 14, '0') +                             // 221 a 234 - CNPJ/CPF Sacado
    PadRight(Pagador.NomeSacado, 30) +                                          // 235 a 264 - Nome do Sacado
    Space(10) +                                                                 // 265 a 274 - Complementação Registro Brancos
    PadRight(Pagador.Logradouro +
        ' ' +
        Pagador.Numero +
        ' ' +
        Pagador.Complemento, 40) +                                              // 275 a 314 - Endereço Sacado
    PadRight(Pagador.Bairro, 12) +                                              // 315 a 326 - Bairro Sacado
    PadRight(Pagador.CEP, 8) +                                                  // 327 a 334 - CEP Sacado
    PadRight(Pagador.Cidade, 15) +                                              // 335 a 349 - Cidade Sacado
    PadRight(Pagador.UF, 2) +                                                   // 350 a 351 - UF Sacado
    PadRight(Pagador.SacadoAvalista.NomeAvalista, 30) +                         // 352 a 381 - Sacador Avalista
    Space(4) +                                                                  // 382 a 385 - Complementação Registro Brancos
    Space(6) +                                                                  // 386 a 391 -  Brancos
    PadLeft(IntToStr(LTitulo.DiasDeProtesto), 2, '0') +                         // 392 a 393 - Dias de Protesto
    '0' +                                                                       // 394 a 394 - Moeda Nacional
    IntToStrZero(ARemessa.Count + 1, 6);                                        // 395 a 400 - Seqüencial

  ARemessa.Add(UpperCase(LLinha));

  LLinha := MontaInstrucoesCNAB400(ACBrTitulo, ARemessa.Count);

  if ACBrTitulo.ListaDadosNFe.Count > 0 then
      GerarRegistrosNFe(ACBrTitulo, aRemessa);

  if (Pagador.SacadoAvalista.CNPJCPF <> '') then
  begin

   LLinha := '5' +                                                              // 001 - 001 Tipo de registro - 5 IDENTIFICAÇÃO DO REGISTRO TRANSAÇÃO
     Space(120) +                                                               // 002 - 121 Completo Registro Brancos
     PadLeft(DefineTipoSacadoAvalista(ACBrTitulo), 2, '0') +                    // 122 - 123 Código de Inscrição
     PadLeft(OnlyNumber(Pagador.SacadoAvalista.CNPJCPF), 14, '0') +             // 124 - 137 Número de Inscrição
     PadRight(Pagador.SacadoAvalista.Logradouro +
       ' ' +
       Pagador.SacadoAvalista.Numero +
       ' ' +
       Pagador.SacadoAvalista.Complemento , 40, ' ') +                          // 138 - 177 Logradouro
     PadRight(Pagador.SacadoAvalista.Bairro, 12, ' ') +                         // 178 - 189 Bairro
     PadLeft(OnlyNumber(Pagador.SacadoAvalista.CEP), 8, '0') +                  // 190 - 197 Cep
     PadRight(Pagador.SacadoAvalista.Cidade, 15, ' ') +                         // 198 - 212 Cidade
     PadRight(Pagador.SacadoAvalista.UF, 2, ' ') +                              // 213 - 214 UF
     Space(180) +                                                               // 215 - 394 Brancos
     IntToStrZero(ARemessa.Count + 1 , 6);                                      // 395 - 400 Sequencial

    aRemessa.Add(UpperCase(LLinha));
  end;
end;

function TACBrBancoFibra.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamentoTodaRede);
end;

function TACBrBancoFibra.MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo; const nRegistro: Integer): String;
begin
  Result := '';

    if ACBrTitulo.Mensagem.Count = 0 then
    begin
      Result := '';
      Exit;
    end;

    Result := '2' +                                                             // 001 a 001 IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO
              '0';                                                              // 002 a 002 ZERO

    if ACBrTitulo.Mensagem.Count >= 0 then
      Result := Result + Copy(PadRight(ACBrTitulo.Mensagem[ 0 ], 69, ' '), 1, 69);         // 003 a 071 CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    if ACBrTitulo.Mensagem.Count >  1 then
      Result := Result + Copy(PadRight(ACBrTitulo.Mensagem[ 1 ], 69, ' '), 1, 69);         // 072 a 140 CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    if ACBrTitulo.Mensagem.Count >  2 then
      Result := Result + Copy(PadRight(ACBrTitulo.Mensagem[ 2 ], 69, ' '), 1, 69);         // 141 a 209 CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    if ACBrTitulo.Mensagem.Count >  3 then
      Result := Result + Copy(PadRight(ACBrTitulo.Mensagem[ 3 ], 69, ' '), 1, 69);         // 210 a 278 CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    if ACBrTitulo.Mensagem.Count >  4 then
      Result := Result + Copy(PadRight(ACBrTitulo.Mensagem[ 4 ], 69, ' '), 1, 69);         // 279 a 347 CONTEÚDO DA 5ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO


    Result := PadRight(Result,392,' ') +
              IntToStrZero(nRegistro + 1, 6);                                   // 395 a 400 - Sequencial

end;

function TACBrBancoFibra.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String;
var
  LDocto: String;
begin
   Result := '0';
   LDocto := PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4, '0') +
     PadLeft(DefineCarteira(ACBrTitulo), 3, '0') +
     PadLeft(ACBrTitulo.NossoNumero, 10, '0');

   Modulo.MultiplicadorInicial := 2;
   Modulo.MultiplicadorFinal   := 1;
   Modulo.MultiplicadorAtual   := 2;
   Modulo.FormulaDigito := frModulo10;
   Modulo.Documento:= LDocto;
   Modulo.Calcular;
   Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoFibra.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result := toRetornoRegistroConfirmado;
    03 : Result := toRetornoRegistroRecusado;
    04 : Result := toRetornoAlteracaoDadosNovaEntrada;
    05 : Result := toRetornoAlteracaoDadosBaixa;
    06 : Result := toRetornoLiquidado;
    07 : Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    08 : Result := toRetornoLiquidadoEmCartorio;
    09 : Result := toRetornoBaixaSimples;
    10 : Result := toRetornoBaixadoViaArquivo;
    12 : Result := toRetornoAbatimentoConcedido;
    13 : Result := toRetornoAbatimentoCancelado;
    14 : Result := toRetornoVencimentoAlterado;
    15 : Result := toRetornoBaixaRejeitada;
    16 : Result := toRetornoInstrucaoRejeitada;
    17 : Result := toRetornoAlteracaoDadosRejeitados;
    19 : Result := toRetornoRecebimentoInstrucaoProtestar;
    20 : Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21 : Result := toRetornoConfInstrucaoNaoProtestar;
    23 : Result := toRetornoEncaminhadoACartorio;
    32 : Result := toRetornoBaixaPorProtesto;
    35 : Result := toRetornoAlegacaoDoSacado;
    36 : Result := toRetornoCustasEdital;
    37 : Result := toRetornoCustasSustacaoJudicial;
    38 : Result := toRetornoSustadoJudicial;
    65 : Result := toRetornoChequePendenteCompensacao;
    69 : Result := toRetornoChequeDevolvido;
    71 : Result := toRetornoEstornoProtesto;
    72 : Result := toRetornoBaixaAutomatica;
    74 : Result := toRetornoConfirmacaoCancelamentoBaixaAutomatica;
    75 : Result := toRetornoLiquidadoParcialmente;
    90 : Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    95 : Result := toRetornoAlteracaoUsoCedente;
    96 : Result := toRetornoTarifaExtratoPosicao;
    97 : Result := toRetornoCustasSustacao;
    98 : Result := toRetornoTarifaInstrucao;
    99 : Result := toRetornoCustasProtesto;
    else
      Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoFibra.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRemessaBaixar;                                              //Pedido de Baixa

    04 : Result := toRemessaConcederAbatimento;                                 //Concessão de Abatimento

    06 : Result := toRemessaAlterarVencimento;                                  //Alteração Vencimento

    09 : Result := toRemessaProtestar;                                          //Pedido de Protesto

    18 : Result := toRemessaCancelarInstrucaoProtestoBaixa;                     //Sustar Protesto e Baixar o Tíulo

    else
      Result := toRemessaRegistrar;                                             //Remessa
  end;
end;

function TACBrBancoFibra.TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
    toRemessaBaixar:
      Result := '02';                                                           //Pedido de Baixa
    toRemessaConcederAbatimento:
      Result := '04';                                                           //Concessão de Abatimento
    toRemessaAlterarVencimento:
      Result := '06';                                                           //Alteração Vencimento
    toRemessaProtestar:
      Result := '09';                                                           //Pedido de Protesto
    toRemessaCancelarInstrucaoProtestoBaixa:
      Result := '18';                                                           //Sustar Protesto e Baixar o Tíulo
    else
      Result := '01';                                                           //Remessa
  end;
end;

end.
