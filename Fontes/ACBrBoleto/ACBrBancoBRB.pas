{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Renato Murilo Pavan                           }
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
unit ACBrBancoBRB;
interface
uses
  Classes, SysUtils,
  ACBrBoleto;

type
  { TACBrBancoBanrisul }
  TACBrBancoBRB = class(TACBrBancoClass)
  protected
  public
    constructor create(AOwner: TACBrBanco);
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList); override;
    function MontarChaveASBACE(const ACBrTitulo: TACBrTitulo): string;
    function CalculaDigitosChaveASBACE(const ChaveASBACESemDigito: string): string;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; overload; override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP}dateutils{$ELSE}ACBrD5{$ENDIF},
  StrUtils,
  ACBrUtil;

{ TACBrBancoBRB }

constructor TACBrBancoBRB.create(AOwner: TACBrBanco);
begin
  inherited create(AOwner);
  fpDigito                := 1;
  fpNome                  := 'BRB';
  fpNumero                := 070;
  fpTamanhoMaximoNossoNum := 12;
  fpTamanhoAgencia        := 3;
  fpTamanhoConta          := 6;
  fpTamanhoCarteira       := 3;
end;

function Modulo11(const Valor: string; Base: Integer = 9; Resto: boolean = false): string;
var
  Soma: integer;
  Contador, Peso, Digito: integer;
begin
   Soma := 0;
   Peso := 2;
   for Contador := Length(Valor) downto 1 do
   begin
      Soma := Soma + (StrToInt(Valor[Contador]) * Peso);
      if Peso < Base then
         Peso := Peso + 1
      else
         Peso := 2;
   end;

   if Resto then
      Result := IntToStr(Soma mod 11)
   else
    begin
      Digito := (Soma mod 11);
      if (Digito in [0..1]) or (Digito = 10) then
         Digito := 1
      else
         Digito := 11 - (Soma mod 11);

      Result := IntToStr(Digito);
    end
end;

function TACBrBancoBRB.CalculaDigitosChaveASBACE(const ChaveASBACESemDigito: string): string;
{Calcula os 2 dígitos usados na CHAVE ASBACE - Código usado por bancos estaduais}
var
  Digito1, Digito2: integer;
  function CalcularDigito1(ChaveASBACESemDigito: string): integer;
    {
     Calcula o primeiro dígito.
     O cálculo é parecido com o da rotina Modulo10. Porém, não faz diferença o
     número de dígitos de cada subproduto.
     Se o resultado da operação for 0 (ZERO) o dígito será 0 (ZERO). Caso contrário,
     o dígito será igual a 10 - Resultado.
    }
  var
    Auxiliar, Soma, Contador, Peso, Digito1: integer;
  begin
     Soma := 0;
     Peso := 2;
     ChaveASBACESemDigito := Copy(ChaveASBACESemDigito, 1, 23);

     for Contador := Length(ChaveASBACESemDigito) downto 1 do
     begin
        Auxiliar := (StrToInt(ChaveASBACESemDigito[Contador]) * Peso);
        if Auxiliar > 9 then
           Auxiliar := Auxiliar - 9;

        Soma := Soma + Auxiliar;
        if Peso = 1 then
           Peso := 2
        else
           Peso := 1;
     end;

     Digito1 := Soma mod 10;
     if (Digito1 = 0) then
        Result := Digito1
     else
        Result := 10 - Digito1;
  end;

  function CalcularDigito2(ChaveASBACESemDigito: string; var Digito1: integer):
      integer;
    {Calcula o segundo dígito}
  var
    Digito2: integer;
    ChaveASBACEComDigito1: string;
  begin
     ChaveASBACEComDigito1 := Copy(ChaveASBACESemDigito, 1, 23) + IntToStr(Digito1);
     Digito2 := StrToInt(Modulo11(ChaveASBACEComDigito1, 7, true));

     if Digito2 = 0 then
        Digito2 := 0
     else if Digito2 > 1 then
        Digito2 := 11 - Digito2
     else
      {Se o resto for igual a 1, Dv 2 teria de ser reclaculado com um novo Dv 1}
      begin
        Digito1 := Digito1 + 1;
        {Se, após incrementar o dígito1, ele ficar igual 10, deve-se
         substituí-lo por 0}
        if Digito1 = 10 then
           Digito1 := 0;
        Digito2 := CalcularDigito2(ChaveASBACESemDigito, Digito1);
      end;

     Result := Digito2;
  end;
begin
   Digito1 := CalcularDigito1(ChaveASBACESemDigito);
   Digito2 := CalcularDigito2(ChaveASBACESemDigito, Digito1);
   Result  := IntToStr(Digito1) + IntToStr(Digito2);
end;

function TACBrBancoBRB.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string;
var
  Campo1, FatorVencimento, Valor, ChaveASBACE, DV: string;
begin
   with ACBrTitulo do
   begin
      Campo1 := PadLeft(IntToStr(Numero), 3, '0') + '9';
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);
      Valor := IntToStrZero(Round(ValorDocumento * 100), 10);
      ChaveASBACE := MontarChaveASBACE(ACBrTitulo);
      DV := Modulo11(Campo1 + FatorVencimento + Valor + ChaveASBACE);
   end;

   Result := Campo1 + DV + FatorVencimento + Valor + ChaveASBACE;
end;

function TACBrBancoBRB.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string;
begin
   Result := Copy(MontarChaveASBACE(ACBrTitulo), 14, 12);
end;

function TACBrBancoBRB.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string;
begin
   Result := '000-' +
             PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 3, '0') + '.' +
             FormatFloat('000,000', StrToFloat(ACBrTitulo.ACBrBoleto.Cedente.Conta)) + '-' +
             PadLeft(ACBrTitulo.ACBrBoleto.Cedente.ContaDigito, 1, '0');
end;

procedure TACBrBancoBRB.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList);
var
  wLinha: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
     wLinha := 'DCB'                                             + // Literal DCB
               '001'                                             + // Versão
               '075'                                             + // Arquivo
               PadLeft(OnlyNumber(Agencia), 3, '0')                 + // Agência
               PadLeft(OnlyNumber(Conta), 6, '0') + PadLeft(ContaDigito, 1, '0')   + // Conta
               FormatDateTime('yyyymmdd', Now)                   + // Data de formatação
               FormatDateTime('hhmmss', Now)                     + // Hora da formatação
               IntToStrZero(ACBrBoleto.ListadeBoletos.Count +1,6); // Qtde de registros Header + Detalhe
     aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
  end;
end;

procedure TACBrBancoBRB.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  TipoPessoa: Char;
  TipoDocumento, TipoJuros, fsTipoDesconto, lDataDesconto: String;
  Prazo1, Prazo2, wLinha, lNossoNumero, wAgenciaCB: String;
  wDiasPagto, wInstrucaoLimitePagto : String;
begin
  with ACBrTitulo do
  begin
     { Código Tipo Pessoa }
     case Sacado.Pessoa of
       pFisica  : TipoPessoa := '1';
       pJuridica: TipoPessoa := '2';
     else
       TipoPessoa:='9';
     end;

     { Código Tipo Documento }
     if AnsiUpperCase(EspecieDoc) = 'DM' then //Duplicata Mercantil
        TipoDocumento := '21'
     else if AnsiUpperCase(EspecieDoc) = 'NP' then //Nota Promissória
        TipoDocumento := '22'
     else if AnsiUpperCase(EspecieDoc) = 'RC' then //Recibo
        TipoDocumento := '25'
     else if AnsiUpperCase(EspecieDoc) = 'DP' then //Duplicata Prestação
        TipoDocumento := '31'
     else //Outras
        TipoDocumento:='39';

     { Nosso Número }
     if StrToIntDef(ACBrBoleto.Cedente.Modalidade, 1) in [1..2] then
        lNossoNumero := PadLeft(MontarCampoNossoNumero(ACBrTitulo), 12, '0')
     else
        lNossoNumero := StringOfChar('0', 12);

     { Juros de Mora }
     if ValorMoraJuros > 0 then
      begin
        TipoJuros := '50';
        Instrucao1 := '01'; // 01- Não Dispensar Juros de Mora
        Prazo1 := '01';
      end
     else
      begin
        TipoJuros := '00';
        Instrucao1 := '00'; // 00- Sem Instrução
        Prazo1 := '00';
      end;

     { Multa }
     if PercentualMulta > 0 then
      begin
       Instrucao2 := '03'; // 03- Cobrar multa de ...% sobre o valor do título
       Prazo2 := '01';
      end
     else
      begin
        Instrucao2 := '00'; // 00- Sem Instrução
        Prazo2 := '00';
      end;

      if (DataLimitePagto > 0) then
      begin
        wDiasPagto:= IntToStrZero(DaysBetween(Vencimento, DataLimitePagto),2);

        if Vencimento <> DataLimitePagto then
         wInstrucaoLimitePagto := '94'
        else
         wInstrucaoLimitePagto := '13';

        if (Instrucao1 = '00') then
         begin
	      Instrucao1 := wInstrucaoLimitePagto;
          Prazo1    := wDiasPagto;
         end
        else if (Instrucao2 = '00') then
         begin
          Instrucao2 := wInstrucaoLimitePagto;
          Prazo2    := wDiasPagto;
        end
      end;

     { Descontos }
     if ValorDesconto > 0 then
      begin
        fsTipoDesconto := '53';
        lDataDesconto := FormatDateTime('ddmmyyyy',DataDesconto);
      end
     else
      begin
        fsTipoDesconto := '00';
        lDataDesconto := '00000000';
      end;

     with ACBrBoleto do
     begin
       if (trim(Cedente.Convenio) <> '') then
         wAgenciaCB := Copy(Cedente.Convenio, 1, 4)
       else
         wAgenciaCB := '0050';

        wLinha:= '01'                                                                   + // Identificação do registro
                 PadLeft(Cedente.Agencia, 3, '0')                                       + // Agência
                 PadLeft(Cedente.Conta, 6, '0') + PadLeft(Cedente.ContaDigito, 1, '0')  + // Conta
                 PadRight(OnlyNumber(Sacado.CNPJCPF), 14, ' ')                          + // Código do Sacado
                 PadRight(Sacado.NomeSacado, 35)                                        + // Nome do Sacado
                 PadRight(Sacado.Logradouro + ', '                                      +
                      Sacado.Numero + ' '                                               +
                      Sacado.Complemento, 35)                                           + // Endereço do Sacado
                 PadRight(Sacado.Cidade, 15)                                            + // Cidade do Sacado
                 PadRight(Sacado.UF, 2)                                                 + // UF do sacado
                 PadRight(OnlyNumber(Sacado.CEP), 8, '0')                               + // CEP do sacado
                 TipoPessoa                                                             + // Código Tipo Pessoa 1- Física; 2- Jurídica ou 9- Isenta
                 PadRight(SeuNumero, 13)                                                + // Seu número
                 PadRight(Cedente.Modalidade, 1)                                        + // Cód. carteira cobrança 1- Sem Registro; 2- Com Registro- Impressão Local ou 3- Com Registro- Impressão pelo BRB
                 FormatDateTime('ddmmyyyy', DataDocumento)                              + // Data de Emissão
                 TipoDocumento                                                          + // Código Tipo Documento 21- Duplicata Mercantil; 22- Nota Promissória; 25- Recibo; 31- Duplicata Prestação ou 39- Outros
                 '0'                                                                    + // Código da Natureza 0 - Simples
                 '0'                                                                    + // Código da Condição Pagto 0- No vencimento; 1- À Vista ou 2- Contra Apresentação
                 '02'                                                                   + // Código da Moeda 02- Real; 51- UFIR ou 91- UPDF
                 '070'                                                                  + // Número do Banco
                 wAgenciaCB                                                             + // Número da Agência Cobradora - Confirmar no suporte
                 Space(30)                                                              + // Praça de Cobrança - Confirmar no suporte
                 FormatDateTime('ddmmyyyy', Vencimento)                                 + // Data de vencimento
                 IntToStrZero(Round(ValorDocumento*100), 14)                            + // Valor do título
                 lNossoNumero                                                           + // Nosso número 000000000000 (Se Código da Categoria de Cobrança= 3)
                 TipoJuros                                                              + // Código do Tipo de Juros 00- Sem Juros ('Não Cobrar Juros'); 50-Diário ("Juro de mora ao dia de...") ou 51- Mensal ("Juro de mora ao mês de ...%")
                 FormatCurr('00000000000000', ValorMoraJuros * 100)                     + // Valor do Juros (Nominal/Tx) 00000000000000 (Se não houver Juros)
                 FormatCurr('00000000000000', ValorAbatimento * 100)                    + // Valor do Abatimento (Nominal/Tx) 00000000000000 (Se não houver Abatimento)
                 fsTipoDesconto                                                         + // Código do Desconto 00- Sem Desconto; 52- Diário ("Desconto por dia de...") ou 53- Mensal ("Desconto Mensal de... até..."
                 lDataDesconto                                                          + // Data limite para Desconto 00000000 (Se não houver Desconto)
                 FormatCurr('00000000000000', ValorDesconto * 100)                      + // Valor do Desconto 00000000000000 (Se não houver Desconto)
                 Instrucao1                                                             + // Código da 1º Instrução
                 Prazo1                                                                 + // Prazo da 1º Instrução 00 (Se não houver 1ª Instrução)
                 Instrucao2                                                             + // Código da 2º Instrução
                 Prazo2                                                                 + // Prazo da 2º Instrução 00 (Se não houver 1ª Instrução)
                 FormatCurr('00000', PercentualMulta * 100)                             + // Taxa ref, a uma das duas Inst. 00000 (Se não houver Instrução ou Taxa) Confirmar a formatação - 5% coloquei assim 00500
                 PadRight(Cedente.Nome,40)                                              + // Emitente do Título
                 Space(40)                                                              + // Mensagem Livre (Observações)
                 Space(32)                                                              ; // Brancos

        aRemessa.Text:= aRemessa.Text + AnsiUpperCase(wLinha);
     end;
  end;
end;

procedure TACBrBancoBRB.LerRetorno400(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;
  Linha: String ;
begin
   if AnsiUpperCase(copy(ARetorno.Strings[0],3,7)) <> 'RETORNO' then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+              //|
                                                             Copy(ARetorno[0],97,2)+'/'+              //|
                                                             Copy(ARetorno[0],99,4),0, 'DD/MM/YYYY' );//|

   ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if Copy(Linha,1,1)<> '1' then
         Continue;

      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
         SeuNumero                   := copy(Linha,93,13);
         NumeroDocumento             := copy(Linha,129,12);

         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(copy(Linha,109,2),0));




         DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                                Copy(Linha,113,2)+'/'+
                                                Copy(Linha,115,4),0, 'DD/MM/YYYY' );

         if StrToIntDef(Copy(Linha,149,8),0) <> 0 then
            Vencimento := StringToDateTimeDef( Copy(Linha,149,2)+'/'+
                                               Copy(Linha,151,2)+'/'+
                                               Copy(Linha,153,4),0, 'DD/MM/YYYY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,157,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,219,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,232,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,245,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,206,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,284,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,258,13),0)/100;
         NossoNumero          := Copy(Linha,71,12);
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,180,13),0)/100;

         if StrToIntDef(Copy(Linha,300,8),0) <> 0 then
            DataBaixa:= StringToDateTimeDef( Copy(Linha,300,2)+'/'+
                                               Copy(Linha,302,2)+'/'+
                                               Copy(Linha,304,4),0, 'DD/MM/YYYY' );
      end;
   end;
end;

function TACBrBancoBRB.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    00: Result := toRetornoBaixadoPorDevolucao;
    02: Result := toRetornoRegistroConfirmado;
    05: Result := toRetornoLiquidadoSemRegistro;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoLiquidado;
    //15 - Liquidação Regularizada
    //16 - Liquidação Regularizada C/Registro.
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoBRB.MontarChaveASBACE(const ACBrTitulo: TACBrTitulo): string;
var
  ChaveASBACESemDigito: string;
begin
  if trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade) = '' then
     raise Exception.Create(ACBrStr('Campo Modalidade não informado, impossivel continuar.'));

  ChaveASBACESemDigito := '000';
  ChaveASBACESemDigito := ChaveASBACESemDigito + PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia), 3, '0');
  ChaveASBACESemDigito := ChaveASBACESemDigito + PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Conta), 6, '0');
  ChaveASBACESemDigito := ChaveASBACESemDigito + PadLeft(ACBrTitulo.ACBrBoleto.Cedente.ContaDigito, 1, '0');
  ChaveASBACESemDigito := ChaveASBACESemDigito + PadRight(trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade), 1); //Categoria da Cobrança
  ChaveASBACESemDigito := ChaveASBACESemDigito + PadLeft(IntToStr(StrToInt(ACBrTitulo.NossoNumero)), 6, '0');
  ChaveASBACESemDigito := ChaveASBACESemDigito + PadLeft(IntToStr(Numero), 3, '0');
  Result := ChaveASBACESemDigito + CalculaDigitosChaveASBACE(ChaveASBACESemDigito);
end;

end.


