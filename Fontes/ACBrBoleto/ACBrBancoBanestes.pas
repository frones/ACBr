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

unit ACBrBancoBanestes;

interface

uses
  Classes, SysUtils,
  ACBrBoleto;

type

  { TACBrBancoBanestes }

  TACBrBancoBanestes = class(TACBrBancoClass)
  private
    fASBACE: string;
    function GetASBACE: string;
  protected
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

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils,
  ACBrUtil;

{ TACBrBancoBanestes }

constructor TACBrBancoBanestes.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 3;
   fpNome                  := 'Banestes';
   fpNumero                := 021;
   fpTamanhoMaximoNossoNum := 8;
   fpTamanhoAgencia        := 3;
   fpTamanhoConta          := 11;
   fpTamanhoCarteira       := 2;
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

procedure TACBrBancoBanestes.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha:= '9' + Space(393)                     + // ID Registro
            IntToStrZero( ARemessa.Count + 1, 6);  // Contador de Registros

   aRemessa.Text := aRemessa.Text + UpperCase(wLinha);

end;

procedure TACBrBancoBanestes.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
   ATipoInscricao, TipoBoleto, ATipoAceite: String;
   DigitoNossoNumero, Ocorrencia: String;
   PracaPostagem, aCarteira, Protesto: String;
   TipoSacado, MensagemCedente, wLinha: String;
   TipoAvalista: String;
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
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : Ocorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarNumeroControle          : Ocorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : Ocorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : Ocorrencia := '20'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : Ocorrencia := '18'; {Sustar protesto e manter na carteira}
         toRemessaOutrasOcorrencias              : Ocorrencia := '31'; {Alteração de Outros Dados}
      else
         Ocorrencia := '01';                                           {Remessa}
      end;

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
         atNao :  ATipoAceite := 'N';
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
                  IfThen(ValorMoraJuros > 0, '0', '9')                        +  // Indica se exite Multa ou não
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

         MotivoLinha := 378;
         for i := 0 to 3 do
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
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
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

function TACBrBancoBanestes.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      29: Result := '29-Ocorrência do Sacado';
      33: Result := '33-Confirmação da Alteração dos Dados do Rateio de Crédito';
      34: Result := '34-Confirmação do Cancelamento dos Dados do Rateio de Crédito';
      35: Result := '35-Confirmação do Cancelamento do Débito Automático Agendado';
    end;
  end;

  if (Result <> '') then
    Exit;

  case CodOcorrencia of
    02: Result:= '02-Entrada Confirmada';
    03: Result:= '03-Entrada Rejeitada';
    04: Result:= '04-Transferência de Carteira - Entrada';
    05: Result:= '05-Transferencia Carteira - Baixa';
    06: Result:= '06-Liquidação';
    07: Result:= '07-Confirmação do Recebimento da Instrução de Desconto';
    08: Result:= '08-Confirmação do Recebimento do Cancelamento do Desconto';
    09: Result:= '09-Baixa';
    11: Result:= '11-Título em Carteira(Em Ser)';
    12: Result:= '12-Confirmação do Recebimento de Instruções de Abatimento';
    13: Result:= '13-Confirmação do Recebimento de Instrução de Cancelamento de Abatimento';
    14: Result:= '14-Confirmação do Recebimento de Instrução de Alteração de Vencimento';
    17: Result:= '17-Liquidação após Baixa ou Liquidação de Título não Registrado';
    19: Result:= '19-Confirmação de Recebimento de Instrução de Protesto';
    20: Result:= '20-Confirmação de Recebimento de Instrução de Sustação/Cancelamento de Protesto' ;
    21: Result:= '21-Solicitação de Segunda Via de Instrumento de Protesto';
    22: Result:= '22-Segunda Via de Instrumento de Protesto Emitida pelo Cartório';
    23: Result:= '23-Remessa a Cartório';
    24: Result:= '24-Retirada de Cartório e Manutenção em Carteira';
    25: Result:= '25-Protestado e Baixado';
    26: Result:= '26-Instrução Rejeitada';
    27: Result:= '27-Confirmação do Pedido de Alteração Outros Dados';
    28: Result:= '28-Débito de Tarifas/Custas';
    30: Result:= '30-Alteração de Outros Dados Rejeitada';
    40: Result:= '40-Confirmação da Alteração do Número do Título Dado pelo Cedente';
    42: Result:= '42-Confirmação da Alteração dos Dados do Sacado';
    43: Result:= '43-Confirmação da Alteração dos Dados do Sacador Avalista';
    51: Result:= '51-Título DDA Reconhecido pelo Sacado';
    52: Result:= '52-Título DDA Não Reconhecido pelo Sacado';
    53: Result:= '53-Título DDA Recusado pela CIP';
    98: Result:= '98-Instrução de Protesto Processada';
    99: Result:= '99-Remessa Rejeitada';
  end;
end;

function TACBrBancoBanestes.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      29: Result := toRetornoOcorrenciasDoSacado;
      33: Result := toRetornoAcertoDadosRateioCredito;
      34: Result := toRetornoCancelamentoDadosRateio;
      35: Result := toRetornoDesagendamentoDebitoAutomatico;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

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
      30: Result := toRetornoAlteracaoDadosRejeitados;
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

function TACBrBancoBanestes.TipoOCorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoOcorrenciasDoSacado                    : Result := '29';
      toRetornoAcertoDadosRateioCredito               : Result := '33';
      toRetornoCancelamentoDadosRateio                : Result := '34';
      toRetornoDesagendamentoDebitoAutomatico         : Result := '35';
    end;
  end;

  if (Result <> '') then
    Exit;

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
      toRetornoAlteracaoDadosRejeitados                    : Result := '30';
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
      toRetornoRegistroRecusado, toRetornoEntradaRejeitadaCarne:
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
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoTarifaOcorrencias:
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

      toRetornoLiquidado:
      case CodMotivo of
         01: Result := 'POR SALDO';
         02: Result := 'POR CONTA';
         03: Result := 'NO PROPRIO BANCO';
         04: Result := 'COMPENSAÇÃO ELETRONICA';
         05: Result := 'COMPENSAÇÃO CONVENCIONAL';
         06: Result := 'POR MEIO ELETRÔNICO';
         07: Result := 'DEPOIS DE FERIADO LOCAL';
         08: Result := 'EM CARTÓRIO';

      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoBaixado:
      case CodMotivo of
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

  end;
end;
end.


