{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor Hugo Gonzales - Panda                    }
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

{$i ACBr.inc}
unit ACBrBancoSofisaSantander;

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  ACBrBoleto;

type

  { TACBrBancoSofisaSantander }

  TACBrBancoSofisaSantander = class(TACBrBancoClass)
  private
  protected
    vTotalTitulos : Double;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer) : String; override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  override;

    Procedure LerRetorno400(ARetorno:TStringList); override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
  {$ifdef COMPILER6_UP} DateUtils {$else} ACBrD5 {$endif},
  StrUtils, ACBrBoletoConversao, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TACBrBancoSofisaSantander }

constructor TACBrBancoSofisaSantander.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                 := 7;
   fpNome                   := 'Banco Sofisa';
   fpNumero                 := 033;
   fpTamanhoMaximoNossoNum  := 12;
   fpTamanhoCarteira        := 4;
   fpTamanhoConta           := 10;
   fpNumeroCorrespondente   := 637;
end;

function TACBrBancoSofisaSantander.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal := 9;
   Modulo.Documento := ACBrTitulo.NossoNumero;
   Modulo.Calcular;

   Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoSofisaSantander.MontarCodigoBarras ( const ACBrTitulo: TACBrTitulo) : String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras, DigitoNossoNumero:String;
begin

   with ACBrTitulo.ACBrBoleto do
   begin
      DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      FatorVencimento   := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      CodigoBarras := '033'+'9'+ FatorVencimento +
                       IntToStrZero(Round(ACBrTitulo.ValorDocumento*100),10)       +
                       '9'+ PadLeft(trim(Cedente.CodigoCedente),7,'0')             +
                       PadLeft(ACBrTitulo.NossoNumero + DigitoNossoNumero, 13,'0') +
                       '0'+ PadLeft(trim(Cedente.Modalidade),3,'0');



      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= '033' + '9'+ DigitoCodBarras + Copy(CodigoBarras,5,39);
end;

function TACBrBancoSofisaSantander.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   with ACBrTitulo do
   begin
      case StrToIntDef(Carteira,0) of
         4: Carteira := '101';
         6: Carteira := '201';
         5: Carteira := '102';
      end;
   end;

   Result:= PadLeft(ACBrTitulo.NossoNumero,12,'0')+ ' '+ CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoSofisaSantander.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   if (ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito = '') then
   begin
     Result := ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente
   end
   else
   begin
     Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente
   end;


end;

procedure TACBrBancoSofisaSantander.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  DigitoNossoNumero, Ocorrencia,aEspecie, sChaveNFe :String;
  Protesto, Multa, valorMulta, aAgencia, TipoSacado, wLinha :String;
  aCarteira, I, mensagemBranco, multiplicadorMulta: Integer;
begin

  aCarteira := StrToIntDef(ACBrTitulo.Carteira, 0 );

  if aCarteira = 102  then
    aCarteira:= 5
  else if aCarteira = 201 then
    aCarteira:= 6
  else if aCarteira = 101 then
    aCarteira:= 4;

  if aCarteira = 5 then
    aAgencia := PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia) +
                       ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito,5,'0')
  else
    aAgencia:= '00000';

  vTotalTitulos:= vTotalTitulos+ ACBrTitulo.ValorDocumento;
  with ACBrTitulo do
  begin
    DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

    {Pegando Código da Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar                        : Ocorrencia := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento            : Ocorrencia := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento            : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento             : Ocorrencia := '06'; {Alteração de vencimento}
      toRemessaProtestar                     : Ocorrencia := '09'; {Pedido de protesto}
      toRemessaNaoProtestar                  : Ocorrencia := '10'; {Sustar protesto antes do início do ciclo de protesto}
      toRemessaCancelarInstrucaoProtesto     : Ocorrencia := '18'; {Sustar protesto e manter na carteira}
    else
      Ocorrencia := '01';                                          {Remessa}
    end;

    {Pegando Especie}
    if trim(EspecieDoc) = 'DM' then
      aEspecie:= '01'
    else if trim(EspecieDoc) = 'NP' then
      aEspecie:= '02'
    else if trim(EspecieDoc) = 'NS' then
      aEspecie:= '03'
    else if trim(EspecieDoc) = 'RC' then
      aEspecie:= '05'
    else if trim(EspecieDoc) = 'DS' then
      aEspecie:= '06'
    else if trim(EspecieDoc) = 'LC' then
      aEspecie:= '07'
    else if trim(EspecieDoc) = 'BDP' then
      aEspecie:= '08'
    else if trim(EspecieDoc) = 'BCC' then
      aEspecie:= '19'
    else
      aEspecie := EspecieDoc;

    {Pegando campo Intruções}
    if (DataProtesto > 0) and (DataProtesto > Vencimento) then //and (Instrucao1 = '06') then
    begin
      Protesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento),2);
      if (trim(Instrucao1) <> '06' )  and (trim(Instrucao2) <> '06' ) then
        If Trim(Instrucao1) = '' then
          Instrucao1 := '06'
        else
          Instrucao2 := '06';
    end
    else
      Protesto:= '00';

    {Pegando Dias Multa}
    if (DataMulta > 0) and (DataMulta > Vencimento) then
    begin
      Multa := IntToStrZero(DaysBetween(DataMulta, Vencimento), 2);
    end
    else
      Multa := '00';

    {Define Valor Multa}
    if MultaValorFixo then
      multiplicadorMulta:= 100
    else
      multiplicadorMulta:= 10000;
    valorMulta:= IntToStrZero( round( PercentualMulta * multiplicadorMulta ), 13);

    {Pegando Tipo de Sacado}
    case Sacado.Pessoa of
      pFisica   : TipoSacado := '01';
      pJuridica : TipoSacado := '02';
    else
      TipoSacado := '99'; //TODO: CHECAR OQ FAZER PARA CEDENTE SEM TIPO
    end;

    with ACBrBoleto do
    begin
      sChaveNFe := '';
      if (ACBrTitulo.ListaDadosNFe.Count > 0) then
        sChaveNFe := ACBrTitulo.ListaDadosNFe[0].ChaveNFe;
      sChaveNFe := PadLeft(sChaveNFe,44,'0');

      wLinha:= '1'                                                  +  // 1- ID Registro
        IfThen(Cedente.TipoInscricao = pJuridica,'02','01')         +  // 2 a 3
        PadLeft(trim(OnlyNumber(Cedente.CNPJCPF)),14,'0')           +  // 4 a 17
        PadRight(trim(Cedente.CodigoTransmissao),20)                +  // 18 a 37
        PadRight( SeuNumero ,25,' ')                                +  // 38 a 62
        '00000000000'                                               +  // 63 a 73
        PadLeft(RightStr(NossoNumero,12),12,'0') + DigitoNossoNumero+  // 74 a 86 Cobrança direta Título Correspondente
        Space(3)                                                    +  // 87 a 89 Modalidade de Cobrança com bancos correspondentes.
        IfThen(PercentualMulta > 0,'2','0')                         +  // 90 a 90
        valorMulta                                                  +  // 91 a 103
        Multa                                                       +  // 104 a 105 Número de dias após o vencimento para aplicar a multa
        Space(2)                                                    +  // 106 a 107 Identificação da Operação no Banco
        IntToStr(aCarteira)                                         +  // 108 a 108 Código da Carteira
        Ocorrencia                                                  +  // 109 a 110 Identificação da Ocorrência
        PadRight( NumeroDocumento,10,' ')                           +  // 111 a 120
        FormatDateTime( 'ddmmyy', Vencimento)                       +  // 121 a 126
        IntToStrZero( round( ValorDocumento * 100), 13)             +  // 127 a 139
        '033' + aAgencia                                            +  // 140 a 147
        PadLeft(aEspecie, 2) + 'N'                                  +  // 148 a 150
        FormatDateTime( 'ddmmyy', DataDocumento )                   +  // 151 a 156
        PadLeft(trim(Instrucao1),2,'0')                             +  // 157 a 158
        PadLeft(trim(Instrucao2),2,'0')                             +  // 159 a 160
        IntToStrZero( round(ValorMoraJuros * 100 ), 13)             +  // 161 a 173
        IfThen(DataDesconto < EncodeDate(2000,01,01),
               '000000',
               FormatDateTime( 'ddmmyy', DataDesconto))             +  // 174 a 179
        IntToStrZero( round( ValorDesconto * 100), 13)              +  // 180 a 192
        IntToStrZero( round( ValorIOF * 100 ), 13)                  +  // 193 a 205
        IntToStrZero( round( ValorAbatimento * 100 ), 13)           +  // 206 a 218
        TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')     +  // 219 a 234
        PadRight( Sacado.NomeSacado, 40, ' ')                       +  // 235 a 274
        PadRight( Sacado.Logradouro + ' '+ Sacado.Numero, 40, ' ')  +  // 275 a 314
        PadRight( Sacado.Bairro,12,' ')                             +  // 315 a 326
        PadRight( OnlyNumber(Sacado.CEP) , 8, ' ' )                 +  // 327 a 334
        PadRight( Sacado.Cidade, 15, ' ')                           +
        PadRight( Sacado.UF, 2 )                                    +  // 335 a 351
        PadRight( Sacado.NomeSacado, 30, ' ')                       +  // 352 a 381
        Space(4)                                                    +  // 382 a 385
        Space(6)                                                    +  // 386 a 391
        Protesto + '0'                                              +  // 392 a 394
        IntToStrZero( aRemessa.Count + 1, 6 );                         // 395 a 400


      wLinha:= UpperCase(wLinha);
      if Mensagem.Count > 0 then
      begin
        wLinha:= wLinha + #13#10                         +
                 '2' + '0';
        for I := 0 to Mensagem.Count - 1 do
        begin
          if i = 5  then
            Break;

          wLinha := wLinha +
            PadRight(Mensagem[I],69);

        end;

        mensagemBranco := (5 - i) * 69;

        wLinha := wLinha + Space(mensagemBranco) + Space(47);
        wLinha := wLinha +  IntToStrZero(aRemessa.Count  + 2, 6 );
      end;

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
    end;
  end;
end;

function TACBrBancoSofisaSantander.GerarRegistroHeader240(
  NumeroRemessa: Integer): String;
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;

procedure TACBrBancoSofisaSantander.GerarRegistroHeader400(
  NumeroRemessa: Integer; aRemessa: TStringList);
var
  wLinha: String;
begin
   vTotalTitulos:= 0;
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                        + // 001-001 ID do Registro
               '1'                                        + // 002-002 ID do Arquivo( 1 - Remessa)
               'REMESSA'                                  + // 003-009 Literal de Remessa
               '01'                                       + // 010-011 Código do Tipo de Serviço
               PadRight( 'COBRANCA', 15 )                 + // 012-026 Descrição do tipo de serviço
               PadRight( CodigoTransmissao, 20)           + // 027-046 Codigo da Empresa no Banco
               PadRight( Nome, 30)                        + // 047-076 Nome da Empresa
               '637'                                      + // 077-079 Código
               PadRight('BANCO SOFISA SA', 15)            + // 080-094 Nome do Banco
               FormatDateTime('ddmmyy',Now)               + // 095-100 Data de geração do arquivo
               Space(294)                                 + // 101-394 Brancos
               IntToStrZero(1,6);                           // 395-400 Nr. Sequencial de Remessa

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoSofisaSantander.GerarRegistroTrailler400( ARemessa:TStringList );
var
  wLinha: String;
begin
   wLinha:= '9'                                            + // ID Registro
            Space(393)                                     + // Complmentação do Registro (Brancos)
            IntToStrZero(ARemessa.Count + 1, 6);

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

Procedure TACBrBancoSofisaSantander.LerRetorno400 ( ARetorno: TStringList );
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo : Integer;
  Linha, rCedente, rAgencia, rConta, rDigitoConta, rCNPJCPF : String;
  wCodBanco: Integer;
begin
   wCodBanco := StrToIntDef(copy(ARetorno.Strings[0],77,3),-1);
   if (wCodBanco <> Numero) and (wCodBanco <> fpNumeroCorrespondente) then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente := trim(Copy(ARetorno[0],47,30));
   rAgencia := trim(Copy(ARetorno[1],18,4));
   rConta   := trim(Copy(ARetorno[1],22,8))+ Copy(ARetorno[1],384,1);
   rConta   := PadLeft( OnlyNumber(rConta),fpTamanhoConta,'0');
   rDigitoConta := Copy(ARetorno[1],385,1);

   case StrToIntDef(Copy(ARetorno[1],2,2),0) of
      01: rCNPJCPF := Copy(ARetorno[1],7,11);
      02: rCNPJCPF := Copy(ARetorno[1],4,14);
   else
     rCNPJCPF := Copy(ARetorno[1],4,14);
   end;

   ACBrBanco.ACBrBoleto.DataCreditoLanc :=
     StringToDateTimeDef(Copy(ARetorno[0], 95, 2) + '/' +
                         Copy(ARetorno[0], 97, 2) + '/' +
                         Copy(ARetorno[0], 99, 2), 0, 'dd/mm/yy');

   ValidarDadosRetorno(rAgencia, rConta, rCNPJCPF);
   with ACBrBanco.ACBrBoleto do
   begin
      Cedente.Nome    := rCedente;
      Cedente.CNPJCPF := rCNPJCPF;
      Cedente.Agencia := rAgencia;
      Cedente.AgenciaDigito:= '0';
      Cedente.Conta   := rConta;
      Cedente.ContaDigito:= rDigitoConta;

      DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                           Copy(ARetorno[0],97,2)+'/'+
                                           Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

      case StrToIntDef(Copy(ARetorno[1],2,2),0) of
         01: Cedente.TipoInscricao:= pFisica;
         else
            Cedente.TipoInscricao:= pJuridica;
      end;

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
         SeuNumero   := copy(Linha,38,25);
         NossoNumero := Copy(Linha,95,12);
         Carteira    := Copy(Linha,108,1);

         OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(
                                                        copy(Linha,109,2),0));

         DataOcorrencia:= StringToDateTimeDef(Copy(Linha,111,2)+'/'+
                                              Copy(Linha,113,2)+'/'+
                                              Copy(Linha,115,2),0, 'DD/MM/YY' );

         NumeroDocumento:= Copy(Linha,117,10);

         CodOcorrencia := StrToIntDef(copy(Linha,135,2),0);

         //-|Se a ocorrencia for igual a > 0 - Houve Erros
         if(CodOcorrencia > 0) then
         begin
            if copy(Linha,137,3) <> '   ' then
            begin
               CodMotivo:= StrToIntDef(copy(Linha,137,3),0);
               MotivoRejeicaoComando.Add(copy(Linha,137,3));
               DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                  OcorrenciaOriginal.Tipo,CodMotivo));
            end;

            if copy(Linha,140,3) <> '   ' then
            begin
               CodMotivo:= StrToIntDef(copy(Linha,140,3),0);
               MotivoRejeicaoComando.Add(copy(Linha,137,3));
               DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                  OcorrenciaOriginal.Tipo,CodMotivo));
            end;

            if copy(Linha,143,3) <> '   ' then
            begin
               CodMotivo:= StrToIntDef(copy(Linha,143,3),0);
               MotivoRejeicaoComando.Add(copy(Linha,137,3));
               DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
                                                  OcorrenciaOriginal.Tipo,CodMotivo));
            end;
         end;

         Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                            Copy(Linha,149,2)+'/'+
                                            Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;

         case StrToIntDef(Copy(Linha,174,2),0) of
            01 : EspecieDoc:= 'DM';
            02 : EspecieDoc:= 'NP';
            03 : EspecieDoc:= 'NS';
            05 : EspecieDoc:= 'RC';
            06 : EspecieDoc:= 'DS';
            07 : EspecieDoc:= 'LS';
            08 : EspecieDoc:= 'BDP';
            19 : EspecieDoc:= 'BCC';
         end;

         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;
         ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,202,13),0) +
                                 StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;

         if Copy(Linha,294,1) = 'N' then
            Aceite:=  atNao
         else
            Aceite:=  atSim;

         if StrToIntDef(Copy(Linha,296,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,296,2)+'/'+
                                               Copy(Linha,298,2)+'/'+
                                               Copy(Linha,300,2),0, 'DD/MM/YY' );

         Sacado.NomeSacado:= Copy(Linha,302,36);
      end;
   end;
end;

function TACBrBancoSofisaSantander.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  { Atribuindo Ocorrências divergêntes entre CNAB240 e CNAB400 }
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      17: Result := '17-Liq. Após Baixa/Liq.Tít. não Registrado';
      24: Result := '24-Retirada de Cartório/Manutenção em Carteira';
      25: Result := '25-Protestado e Baixado';
      26: Result := '26-Instrução Rejeitada';
      51: Result := '51-Título DDA Reconhecido Pelo Sacado';
      52: Result := '52-Título DDA Não Reconhecido Pelo Sacado';
      53: Result := '53-Título DDA Recusado Pela CIP';
    end;
  end
  else
  begin
    case CodOcorrencia of
      17: Result := '17-Liquidado em Cartório';
      24: Result := '24-Custas de Cartório';
      25: Result := '25-Protestar Título';
      26: Result := '26-Sustar Protesto';
      35: Result := '35-Título DDA Reconhecido Pelo Sacado';
      36: Result := '36-Título DDA Não Reconhecido Pelo Sacado';
      37: Result := '37-Título DDA Recusado Pela CIP';
    end;
  end;

  if (Result <> '') then
    Exit;

  case CodOcorrencia of
    01: Result := '01-Título Não Existe';
    02: Result := '02-Entrada Tít.Confirmada';
    03: Result := '03-Entrada Tít.Rejeitada';
    04: Result := '04-Transf. de Carteira/Entrada';
    05: Result := '05-Transf. de Carteira/Baixa';
    06: Result := '06-Liquidação';
    07: Result := '07-Liquidação por Conta';
    08: Result := '08-Liquidação por Saldo';
    09: Result := '09-Baixa Automática';
    10: Result := '10-Tít.Baix.Conf.Instrução';
    11: Result := '11-Em Ser';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Prorrogação de Vencimento';
    15: Result := '15-Confirmação de Protesto';
    16: Result := '16-Tít.Já Baixado/Liquidado';
    19: Result := '19-Recebimento da Instrução Protesto';
    20: Result := '20-Recebimento da Instrução Não Protestar';
    21: Result := '21-Tít. Enviado a Cartório';
    22: Result := '22-Tít. Retirado de Cartório';
    23: Result := '23-Remessa a Cartório';
    27: Result := '27-Confirmação alt.de outros dados';
    28: Result := '28-Débito de tarifas e custas';
    29: Result := '29-Ocorrência do sacado';
    30: Result := '30-Alteração de dados rejeitada';
    32: Result := '32-Código IOF Inválido';
    38: Result := '38-Recebimento da Instrução Não Protestar'
  end;
end;

function TACBrBancoSofisaSantander.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
  // DONE -oJacinto Junior: Ajustar para utilizar as ocorrências corretas.
  Result := toTipoOcorrenciaNenhum;

  { Atribuindo Ocorrências divergêntes entre CNAB240 e CNAB400 }
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoProtestado;
      26: Result := toRetornoInstrucaoRejeitada;
      35: Result := toRetornoTituloDDAReconhecidoPagador;
      36: Result := toRetornoTituloDDANaoReconhecidoPagador;
      37: Result := toRetornoTituloDDARecusadoCIP;
    end;
  end
  else
  begin
    case CodOcorrencia of
      17: Result := toRetornoLiquidadoEmCartorio;
      24: Result := toRetornoCustasCartorio;
      25: Result := toRetornoRecebimentoInstrucaoProtestar;
      26: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      51: Result := toRetornoTituloDDAReconhecidoPagador;
      52: Result := toRetornoTituloDDANaoReconhecidoPagador;
      53: Result := toRetornoTituloDDARecusadoCIP;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    01: Result := toRetornoTituloNaoExiste;
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    04: Result := toRetornoTransferenciaCarteiraEntrada;
    05: Result := toRetornoTransferenciaCarteiraBaixa;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoLiquidadoPorConta;
    08: Result := toRetornoLiquidadoSaldoRestante;
    09: Result := toRetornoBaixaAutomatica;
    10: Result := toRetornoBaixadoInstAgencia;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoProtestado;
    16: Result := toRetornoTituloJaBaixado;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoEncaminhadoACartorio;
    22: Result := toRetornoRetiradoDeCartorio;
    23: Result := toRetornoEntradaEmCartorio;
    27: Result := toRetornoAlteracaoUsoCedente;
    28: Result := toRetornoDebitoTarifas;
    29: Result := toRetornoOcorrenciasDoSacado;
    30: Result := toRetornoAlteracaoDadosRejeitados;
    32: Result := toRetornoIOFInvalido;
    38: Result := toRetornoRecebimentoInstrucaoNaoProtestar;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoSofisaSantander.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração do controle do participante}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    18 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar protesto e manter na carteira}
    98 : Result:= toRemessaNaoProtestar;                    {Sustar protesto antes do início do ciclo de protesto}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoSofisaSantander.TipoOcorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoLiquidadoAposBaixaOuNaoRegistro               : Result := '17';
      toRetornoRetiradoDeCartorio                            : Result := '24';
      toRetornoProtestado                                    : Result := '25';
      toRetornoInstrucaoRejeitada                            : Result := '26';
      toRetornoTituloDDAReconhecidoPagador                   : Result := '35';
      toRetornoTituloDDANaoReconhecidoPagador                : Result := '36';
      toRetornoTituloDDARecusadoCIP                          : Result := '37';
    end;
  end
  else
  begin
    case TipoOcorrencia of
      toRetornoLiquidadoEmCartorio                           : Result := '17';
      toRetornoCustasCartorio                                : Result := '24';
      toRetornoRecebimentoInstrucaoProtestar                 : Result := '25';
      toRetornoRecebimentoInstrucaoSustarProtesto            : Result := '26';
      toRetornoTituloDDAReconhecidoPagador                   : Result := '51';
      toRetornoTituloDDANaoReconhecidoPagador                : Result := '52';
      toRetornoTituloDDARecusadoCIP                          : Result := '53';
    end;
  end;

  if (Result <> '') then
    Exit;

  case TipoOcorrencia of
    toRetornoTituloNaoExiste                                 : Result := '01';
    toRetornoRegistroConfirmado                              : Result := '02';
    toRetornoRegistroRecusado                                : Result := '03';
    toRetornoTransferenciaCarteiraEntrada                    : Result := '04';
    toRetornoTransferenciaCarteiraBaixa                      : Result := '05';
    toRetornoLiquidado                                       : Result := '06';
    toRetornoLiquidadoPorConta                               : Result := '07';
    toRetornoLiquidadoSaldoRestante                          : Result := '08';
    toRetornoBaixaAutomatica                                 : Result := '09';
    toRetornoBaixadoInstAgencia                              : Result := '10';
    toRetornoTituloEmSer                                     : Result := '11';
    toRetornoAbatimentoConcedido                             : Result := '12';
    toRetornoAbatimentoCancelado                             : Result := '13';
    toRetornoVencimentoAlterado                              : Result := '14';
    toRetornoProtestado                                      : Result := '15';
    toRetornoTituloJaBaixado                                 : Result := '16';
    toRetornoRecebimentoInstrucaoProtestar                   : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto              : Result := '20';
    toRetornoEncaminhadoACartorio                            : Result := '21';
    toRetornoRetiradoDeCartorio                              : Result := '22';
    toRetornoEntradaEmCartorio                               : Result := '23';
    toRetornoAlteracaoUsoCedente                             : Result := '27';
    toRetornoDebitoTarifas                                   : Result := '28';
    toRetornoOcorrenciasDoSacado                             : Result := '29';
    toRetornoAlteracaoDadosRejeitados                        : Result := '30';
    toRetornoIOFInvalido                                     : Result := '32';
    toRetornoRecebimentoInstrucaoNaoProtestar                : Result := '38';
  else
    Result := '02';
  end;
end;

function TACBrBancoSofisaSantander.CodMotivoRejeicaoToDescricao( const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo: Integer) : String;
begin
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c400) then
  begin
    case CodMotivo of
      001: Result := '001-NOSSO NUMERO NAO NUMERICO';
      002: Result := '002-VALOR DO ABATIMENTO NAO NUMERICO';
      003: Result := '003-DATA VENCIMENTO NAO NUMERICA';
      004: Result := '004-CONTA COBRANCA NAO NUMERICA';
      005: Result := '005-CODIGO DA CARTEIRA NAO NUMERICO';
      006: Result := '006-CODIGO DA CARTEIRA INVALIDO';
      007: Result := '007-ESPECIE DO DOCUMENTO INVALIDA';
      008: Result := '008-UNIDADE DE VALOR NAO NUMERICA';
      009: Result := '009-UNIDADE DE VALOR INVALIDA';
      010: Result := '010-CODIGO PRIMEIRA INSTRUCAO NAO NUMERICA';
      011: Result := '011-CODIGO SEGUNDA INSTRUCAO NAO NUMERICA';
      012: Result := '012-VALOR DO TITULO EM OUTRA UNIDADE';
      013: Result := '013-VALOR DO TITULO NAO NUMERICO';
      014: Result := '014-VALOR DE MORA NAO NUMERICO';
      015: Result := '015-DATA EMISSAO NÃO NUMERICA';
      016: Result := '016-DATA DE VENCIMENTO INVALIDA';
      017: Result := '017-CODIGO DA AGENCIA COBRADORA NAO NUMERICA';
      018: Result := '018-VALOR DO IOC NAO NUMERICO';
      019: Result := '019-NUMERO DO CEP NAO NUMERICO';
      020: Result := '020-TIPO INSCRICAO NAO NUMERICO';
      021: Result := '021-NUMERO DO CGC OU CPF NAO NUMERICO';
      022: Result := '022-CODIGO OCORRENCIA INVALIDO';
      024: Result := '024-TOTAL PARCELA NAO NUMERICO';
      025: Result := '025-VALOR DESCONTO NAO NUMERICO';
      026: Result := '026-CODIGO BANCO COBRADOR INVALIDO';
      027: Result := '027-NUMERO PARCELAS CARNE NAO NUMERICO';
      028: Result := '028-NUMERO PARCELAS CARNE ZERADO';
      029: Result := '029-VALOR DE MORA INVALIDO';
      030: Result := '030-DT VENC MENOR DE 15 DIAS DA DT PROCES';
      039: Result := '039-PERFIL NAO ACEITA TITULO EM BCO CORRESP';
      041: Result := '041-AGENCIA COBRADORA NAO ENCONTRADA';
      042: Result := '042-CONTA COBRANCA INVALIDA';
      043: Result := '043-NAO BAIXAR,  COMPL. INFORMADO INVALIDO';
      044: Result := '044-NAO PROTESTAR, COMPL. INFORMADO INVALIDO';
      045: Result := '045-QTD DE DIAS DE BAIXA NAO PREENCHIDO';
      046: Result := '046-QTD DE DIAS PROTESTO NAO PREENCHIDO';
      047: Result := '047-TOT PARC. INF. NAO BATE C/ QTD PARC GER';
      048: Result := '048-CARNE COM PARCELAS COM ERRO';
      049: Result := '049-SEU NUMERO NAO CONFERE COM O CARNE';
      051: Result := '051-TITULO NAO ENCONTRADO';
      052: Result := '052-OCOR.  NAO ACATADA, TITULO  LIQUIDADO';
      053: Result := '053-OCOR. NAO ACATADA, TITULO BAIXADO';
      054: Result := '054-TITULO COM ORDEM DE PROTESTO JA EMITIDA';
      055: Result := '055-OCOR. NAO ACATADA, TITULO JA PROTESTADO';
      056: Result := '056-OCOR. NAO ACATADA, TIT. NAO VENCIDO';
      057: Result := '057-CEP DO SACADO INCORRETO';
      058: Result := '058-CGC/CPF INCORRETO';
      059: Result := '059-INSTRUCAO ACEITA SO P/ COBRANCA SIMPLES';
      060: Result := '060-ESPECIE DOCUMENTO NAO PROTESTAVEL';
      061: Result := '061-CEDENTE SEM CARTA DE PROTESTO';
      062: Result := '062-SACADO NAO PROTESTAVEL';
      063: Result := '063-CEP NAO ENCONTRADO NA TABELA DE PRACAS';
      064: Result := '064-TIPO DE COBRANCA NAO PERMITE PROTESTO';
      065: Result := '065-PEDIDO SUSTACAO JA SOLICITADO';
      066: Result := '066-SUSTACAO PROTESTO FORA DE PRAZO';
      067: Result := '067-CLIENTE NAO TRANSMITE REG. DE OCORRENCIA';
      068: Result := '068-TIPO DE VENCIMENTO INVALIDO';
      069: Result := '069-PRODUTO DIFERENTE DE COBRANCA SIMPLES';
      070: Result := '070-DATA PRORROGACAO MENOR QUE DATA VENCTO';
      071: Result := '071-DATA ANTECIPACAO MAIOR QUE DATA VENCTO';
      072: Result := '072-DATA DOCUMENTO SUPERIOR A DATA INSTRUCAO';
      073: Result := '073-ABATIMENTO MAIOR/IGUAL AO VALOR TITULO';
      074: Result := '074-PRIM. DESCONTO MAIOR/IGUAL VALOR TITULO';
      075: Result := '075-SEG. DESCONTO MAIOR/IGUAL VALOR TITULO';
      076: Result := '076-TERC. DESCONTO MAIOR/IGUAL VALOR TITULO';
      077: Result := '077-DESC. POR ANTEC. MAIOR/IGUAL VLR TITULO';
      078: Result := '078-NAO EXISTE ABATIMENTO P/ CANCELAR';
      079: Result := '079-NAO EXISTE PRIM. DESCONTO P/ CANCELAR';
      080: Result := '080-NAO EXISTE SEG. DESCONTO P/ CANCELAR';
      081: Result := '081-NAO EXISTE TERC. DESCONTO P/ CANCELAR';
      082: Result := '082-NAO EXISTE DESC. POR ANTEC. P/ CANCELAR';
      084: Result := '084-JA EXISTE SEGUNDO DESCONTO';
      085: Result := '085-JA EXISTE TERCEIRO DESCONTO';
      086: Result := '086-DATA SEGUNDO DESCONTO INVALIDA';
      087: Result := '087-DATA TERCEIRO DESCONTO INVALIDA';
      089: Result := '089-DATA MULTA MENOR/IGUAL QUE VENCIMENTO';
      090: Result := '090-JA EXISTE DESCONTO POR DIA ANTECIPACAO';
      091: Result := '091-JA EXISTE CONCESSAO DE DESCONTO';
      092: Result := '092-NOSSO NUMERO JA CADASTRADO';
      093: Result := '093-VALOR DO TITULO NAO INFORMADO';
      094: Result := '094-VALOR TIT. EM OUTRA MOEDA NAO INFORMADO';
      095: Result := '095-PERFIL NAO ACEITA VALOR TITULO ZERADO';
      096: Result := '096-ESPECIE DOCTO NAO PERMITE PROTESTO';
      097: Result := '097-ESPECIE DOCTO NAO PERMITE IOC ZERADO';
      098: Result := '098-DATA EMISSAO INVALIDA';
      099: Result := '099-REGISTRO DUPLICADO NO MOVIMENTO DIÁRIO';
      100: Result := '100-DATA EMISSAO MAIOR QUE A DATA VENCIMENTO';
      101: Result := '101-NOME DO SACADO NÃO INFORMADO';
      102: Result := '102-ENDERECO DO SACADO NÃO INFORMADO';
      103: Result := '103-MUNICIPIO DO SACADO NAO INFORMADO';
      104: Result := '104-UNIDADE DA FEDERACAO NAO INFORMADA';
      105: Result := '105-TIPO INSCRICAO NÃO EXISTE';
      106: Result := '106-CGC/CPF NAO INFORMADO';
      107: Result := '107-UNIDADE DA FEDERACAO INCORRETA';
      108: Result := '108-DIGITO CGC/CPF INCORRETO';
      109: Result := '109-VALOR MORA TEM QUE SER ZERO (TIT = ZERO)';
      110: Result := '110-DATA PRIMEIRO DESCONTO INVALIDA';
      111: Result := '111-DATA  DESCONTO NAO NUMERICA';
      112: Result := '112-VALOR DESCONTO NAO INFORMADO';
      113: Result := '113-VALOR DESCONTO INVALIDO';
      114: Result := '114-VALOR ABATIMENTO NAO INFORMADO';
      115: Result := '115-VALOR ABATIMENTO MAIOR VALOR TITULO';
      116: Result := '116-DATA MULTA NAO NUMERICA';
      117: Result := '117-VALOR DESCONTO MAIOR VALOR TITULO';
      118: Result := '118-DATA MULTA NAO INFORMADA';
      119: Result := '119-DATA MULTA MAIOR QUE DATA DE VENCIMENTO';
      120: Result := '120-PERCENTUAL MULTA NAO NUMERICO';
      121: Result := '121-PERCENTUAL MULTA NAO INFORMADO';
      122: Result := '122-VALOR IOF MAIOR QUE VALOR TITULO';
      123: Result := '123-CEP DO SACADO NAO NUMERICO';
      124: Result := '124-CEP SACADO NAO ENCONTRADO';
      126: Result := '126-CODIGO P. BAIXA / DEVOL. INVALIDO';
      127: Result := '127-CODIGO P. BAIXA / DEVOL. NAO NUMERICA';
      128: Result := '128-CODIGO PROTESTO INVALIDO';
      129: Result := '129-ESPEC DE DOCUMENTO NAO NUMERICA';
      130: Result := '130-FORMA DE CADASTRAMENTO NAO NUMERICA';
      131: Result := '131-FORMA DE CADASTRAMENTO INVALIDA';
      132: Result := '132-FORMA CADAST. 2 INVALIDA PARA CARTEIRA 3';
      133: Result := '133-FORMA CADAST. 2 INVALIDA PARA CARTEIRA 4';
      134: Result := '134-CODIGO DO MOV. REMESSA NAO NUMERICO';
      135: Result := '135-CODIGO DO MOV. REMESSA INVALIDO';
      136: Result := '136-CODIGO BCO NA COMPENSACAO NAO NUMERICO';
      138: Result := '138-NUM. LOTE REMESSA(DETALHE) NAO NUMERICO';
      140: Result := '140-COD. SEQUEC.DO REG. DETALHE INVALIDO';
      141: Result := '141-NUM. SEQ. REG. DO LOTE NAO NUMERICO';
      142: Result := '142-NUM.AG.CEDENTE/DIG.NAO NUMERICO';
      144: Result := '144-TIPO DE DOCUMENTO NAO NUMERICO';
      145: Result := '145-TIPO DE DOCUMENTO INVALIDO';
      146: Result := '146-CODIGO P. PROTESTO NAO NUMERICO';
      147: Result := '147-QTDE DE DIAS P. PROTESTO INVALIDO';
      148: Result := '148-QTDE DE DIAS P. PROTESTO NAO NUMERICO';
      149: Result := '149-CODIGO DE MORA INVALIDO';
      150: Result := '150-CODIGO DE MORA NAO NUMERICO';
      151: Result := '151-VL.MORA IGUAL A ZEROS P. COD.MORA 1';
      152: Result := '152-VL. TAXA MORA IGUAL A ZEROS P.COD MORA 2';
      154: Result := '154-VL. MORA NAO NUMERICO P. COD MORA 2';
      155: Result := '155-VL. MORA INVALIDO P. COD.MORA 4';
      156: Result := '156-QTDE DIAS P.BAIXA/DEVOL. NAO NUMERICO';
      157: Result := '157-QTDE DIAS BAIXA/DEV. INVALIDO P. COD. 1';
      158: Result := '158-QTDE DIAS BAIXA/DEV. INVALIDO P.COD. 2';
      160: Result := '160-BAIRRO DO SACADO NAO INFORMADO';
      161: Result := '161-TIPO INSC.CPF/CGC SACADOR/AVAL.NAO NUM.';
      162: Result := '162-INDICADOR DE CARNE NAO NUMERICO';
      163: Result := '163-NUM. TOTAL DE PARC.CARNE NAO NUMERICO';
      164: Result := '164-NUMERO DO PLANO NAO NUMERICO';
      165: Result := '165-INDICADOR DE PARCELAS CARNE INVALIDO';
      166: Result := '166-N.SEQ. PARCELA INV.P.INDIC. MAIOR 0';
      167: Result := '167-N. SEQ.PARCELA INV.P.INDIC.DIF.ZEROS';
      168: Result := '168-N.TOT.PARC.INV.P.INDIC. MAIOR ZEROS';
      169: Result := '169-NUM.TOT.PARC.INV.P.INDIC.DIFER.ZEROS';
      170: Result := '170-FORMA DE CADASTRAMENTO 2 INV.P.CART.5';
      199: Result := '199-TIPO INSC.CGC/CPF SACADOR.AVAL.INVAL.';
      200: Result := '200-NUM.INSC.(CGC)SACADOR/AVAL.NAO NUMERICO';
      201: Result := '201-ALT. DO CONTR. PARTICIPANTE INVALIDO';
      202: Result := '202-ALT. DO SEU NUMERO INVALIDA';
      218: Result := '218-BCO COMPENSACAO NAO NUMERICO (D3Q)';
      219: Result := '219-BCO COMPENSACAO INVALIDO (D3Q)';
      220: Result := '220-NUM. DO LOTE REMESSA NAO NUMERICO(D3Q)';
      221: Result := '221-NUM. SEQ. REG. NO LOTE (D3Q)';
      222: Result := '222-TIPO INSC.SACADO NAO NUMERICO (D3Q)';
      223: Result := '223-TIPO INSC.SACADO INVALIDO (D3Q)';
      224: Result := '224-NUM.INSC.SACADO NAO NUMERICO (D3Q)';
      225: Result := '225-NUM.INSC.SAC.INV.P.TIPO INSC.0 E 9(D3Q)';
      226: Result := '226-NUM.BCO COMPENSACAO NAO NUMERICO (D3R)';
      228: Result := '228-NUM. LOTE REMESSA NAO NUMERICO (D3R)';
      229: Result := '229-NUM. SEQ. REG. LOTE NAO NUMERICO (D3R)';
      246: Result := '246-COD.BCO COMPENSACAO NAO NUMERICO (D3S)';
      247: Result := '247-COD. BANCO COMPENSACAO INVALIDO (D3S)';
      248: Result := '248-NUM.LOTE REMESSA NAO NUMERICO (D3S)';
      249: Result := '249-NUM.SEQ.DO REG.LOTE NAO NUMERICO (D3S)';
      250: Result := '250-NUM.IDENT.DE IMPRESSAO NAO NUMERICO(D3S)';
      251: Result := '251-NUM.IDENT.DE IMPRESSAO INVALIDO (D3S)';
      252: Result := '252-NUM.LINHA IMPRESSA NAO NUMERICO(D3S)';
      253: Result := '253-COD.MSG. P.REC. SAC. NAO NUMERICO (D3S)';
      254: Result := '254-COD.MSG.P.REC.SACADO INVALIDO(D3S)';
      258: Result := '258-VL.MORA NAO NUMERICO P.COD=4(D3P)';
      259: Result := '259-CAD.TXPERM.SK.INV.P.COD.MORA=4(D3P)';
      260: Result := '260-VL.TIT(REAL).INV.P.COD.MORA = 1(DEP)';
      261: Result := '261-VL.OUTROS INV.P.COD.MORA = 1(D3P)';
    else
      Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
    end;
  end
  else // 240
  begin
    case TipoOcorrencia of
    toRetornoComandoRecusado, toRetornoRegistroRecusado: //03 (Entrada rejeitada)
      case CodMotivo of
        01: Result:='Codigo do banco invalido';
        02: Result:='Codigo do registro detalhe invalido';
        03: Result:='Codigo do segmento invalido';
        04: Result:='Codigo do movimento nao permitido para carteira';
        05: Result:='Codigo de movimento invalido';
        06: Result:='Tipo/numero de inscricao do beneficiário invalidos';
        07: Result:='Agencia/Conta/DV invalido';
        08: Result:='Nosso numero invalido';
        09: Result:='Nosso numero duplicado';
        10: Result:='Carteira invalida';
        11: Result:='Forma de cadastramento do titulo invalido';
        12: Result:='Tipo de documento invalido';
        13: Result:='Identificacao da emissao do bloqueto invalida';
        14: Result:='Identificacao da distribuicao do bloqueto invalida';
        15: Result:='Caracteristicas da cobranca incompativeis';
        16: Result:='Data de vencimento invalida';
        17: Result:='Data de vencimento anterior a data de emissao';
        18: Result:='Vencimento fora do prazo de operacao';
        19: Result:='Titulo a cargo de Bancos Correspondentes com vencimento inferior XX dias';
        20: Result:='Valor do titulo invalido';
        21: Result:='Especie do titulo invalida';
        22: Result:='Especie nao permitida para a carteira';
        23: Result:='Aceite invalido';
        24: Result:='Data da emissao invalida';
        25: Result:='Data da emissao posterior a data';
        26: Result:='Codigo de juros de mora invalido';
        27: Result:='Valor/Taxa de juros de mora invalido';
        28: Result:='Codigo do desconto invalido';
        29: Result:='Valor do desconto maior ou igual ao valor do titulo ';
        30: Result:='Desconto a conceder nao confere';
        31: Result:='Concessao de desconto - ja existe desconto anterior';
        32: Result:='Valor do IOF invalido';
        33: Result:='Valor do abatimento invalido';
        34: Result:='Valor do abatimento maior ou igual ao valor do titulo';
        35: Result:='Abatimento a conceder nao confere';
        36: Result:='Concessao de abatimento - ja existe abatimento anterior';
        37: Result:='Codigo para protesto invalido';
        38: Result:='Prazo para protesto invalido';
        39: Result:='Pedido de protesto nao permitido para o titulo';
        40: Result:='Titulo com ordem de protesto emitida';
        41: Result:='Pedido de cancelamento/sustacao para titulos sem instrucao de protesto';
        42: Result:='Codigo para baixa/devolucao invalido';
        43: Result:='Prazo para baixa/devolucao invalido';
        44: Result:='Codigo da moeda invalido';
        45: Result:='Nome do pagador nao informado';
        46: Result:='Tipo/numero de inscricao do pagador invalidos';
        47: Result:='Endereco do pagador nao informado';
        48: Result:='CEP invalido';
        49: Result:='CEP sem praca de cobranca /nao localizado';
        50: Result:='CEP referente a um Banco Correspondente';
        51: Result:='CEP incompativel com a unidade da federacao';
        52: Result:='Unidade da federacao invalida';
        53: Result:='Tipo/numero de inscricao do sacador/avalista invalidos';
        54: Result:='Sacador/Avalista nao informado';
        55: Result:='Nosso numero no Banco Correspondente nao informado';
        56: Result:='Codigo do Banco Correspondente nao informado';
        57: Result:='Codigo da multa invalido';
        58: Result:='Data da multa invalida';
        59: Result:='Valor/Percentual da multa invalido';
        60: Result:='Movimento para titulo nao cadastrado';
        61: Result:='Alteracao da agencia cobradora/dv invalida';
        62: Result:='Tipo de impressao invalido';
        63: Result:='Entrada para titulo ja cadastrado';
        64: Result:='Numero da linha invalido';
        65: Result:='A espécie de título não permite a instrução';
        72: Result:='Entrada de título Sem Registro';
        90: Result:='Identificador/Quantidade de Parcelas de carnê invalido';
        92: Result:='Data de Desconto Inválida';
      end;
    toRetornoLiquidadoSemRegistro, toRetornoLiquidado, toRetornoLiquidadoPorConta,
       toRetornoLiquidadoSaldoRestante, toRetornoLiquidadoEmCartorio: // 05, 06, 07, 08 e 15 (Liquidado)
      case CodMotivo of
        01: Result:='01-Por saldo';
        02: Result:='02-Por conta';
        03: Result:='03-No próprio banco';
        04: Result:='04-Compensação eletrônica';
        05: Result:='05-Compensação convencional';
        06: Result:='06-Arquivo magnético';
        07: Result:='07-Após feriado local';
        08: Result:='08-Em cartório';
        09: Result:='09-Pagamento Parcial';
      end;
    else
      Result := IntToStrZero(CodMotivo, 2) + ' - Outros Motivos';
    end; //case TipoOcorrencia

  end; //else 240

end;

end.
