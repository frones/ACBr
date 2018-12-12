{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 28/06/2004:  Daniel Simoes de Almeida
|*   Inicio do desenvolvimento
|* 31/05/2005:  Daniel Simoes de Almeida
|*   Variaveis Subtotal ou TotalPago poderiam, conter valores com mais de 2
|*   decimais, o que poderia causar problemas nas comparações dos valores.
|*   -  Bug reportado por Licerio Jose Rodrigues Neto
|* 28/07/2005:  Daniel Simoes de Almeida
|*   Leitura do comando de Abertura de gaveta não era decodificada com AscToString
|*   -  Bug reportado por Fabio Farias
|* 05/08/2005:  Daniel Simoes de Almeida
|*  - Propriedade "Operador" será gravada no arquivo ACBrECFxxx.INI
|*  - Rodapé padrão modificado para incluir as Informações: Versao do ACBr e
|*  Operador
|* 12/08/2005:  Daniel Simoes de Almeida
|*  - Troco no cupom não era subtraido da forma de Pagamento Dinheiro
|* 27/10/2005: Daniel Simoes de Almeida
|*  - Modificado o Texto impresso apos o termino das Formas de pagamento de
|*    "Valor Recebido" para "S O M A"...
|*    - Essa linha será impressa apenas se tiver mais de 1 pagamento no Cupom
|* 22/11/2005: Daniel Simoes de Almeida
|*  - Relatorio Gerencial não imprimia a primeira linha
|*  - Relatorio Gerencial modificado para imprimir apenas um cabeçalho,
|*    semelhante aos ECFs novos (não impreme mais uma Leitura X)
|*  - Corrigido Consumo excessivo de CPU ao vender Muitos Itens
|* 29/11/2005: Daniel Simoes de Almeida
|*  - Corrigido bug no retorno da propriedade NomeArqINI
|* 03/05/2006:  Daniel Simoes de Almeida
|*  - Usando Arquivos ou portas paralelas, em algumas ocasioes ocorria erro de
|*    I/O ao Ativar     (bug reportado por Fabio Farias)
|* 06/02/2007:  Thiago Delgado Pinto
|*  - Adicionada a função "NumeroColunas", para facilitar a impressao de linhas
|*    com expandido
|* 21/02/2007:  Marcos Agostini
|*  - Corrigido Bug no Cancelamento de Item com Desconto
|* 06/03/2007:  Fabio Farias
|*  - Corrigido Bug na Abertura de Cupom Fiscal Vinculado, quando Cupom anterior
|*    continha mais de uma forma de Pagamento
|* 01/04/2007:  Daniel Simoes de Almeida
|*  - Implementados métodos de Cupom Não Fiscal
|* 11/05/2007:  Fabio Farias
|*  - Corrigido Bug no Cancelamento de Item (arredondamento)
|* 24/12/2008:  Daniel Simoes de Almeida
|*  - Implementado retorno da propriedade DataMovimento (por Waldir Paim)
|*  - Identificação do Cliente no rodapé do Cupom estava duplicando
|*    as informações, corrigido
******************************************************************************}

{$I ACBr.inc}

unit ACBrECFNaoFiscal ;

interface
uses Classes, Contnrs, Math, SysUtils, IniFiles,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF}
     {$IFNDEF NOGUI}
        {$IF DEFINED(VisualCLX)}
           ,QControls, QForms, QDialogs
        {$ELSEIF DEFINED(FMX)}
           ,FMX.Controls, FMX.Forms, FMX.Dialogs, System.UITypes
        {$ELSE}
           ,Controls, Forms, Dialogs
          {$IFDEF DELPHIXE2_UP}
           , System.UITypes
          {$ENDIF}  
        {$IFEND}
     {$ENDIF},
     ACBrECFClass, ACBrDevice, ACBrConsts;

const
      cCmdImpCondensado = #15 ;
      cCmdImpExpandidoUmaLinha = #14 ;
      cCmdImpFimExpandido = '#20';
      cCmdImpZera = #27+'@' ;

type

TACBrECFNaoFiscalAliquota = class
 private
    fsAliquota: Double ;
    fsISS: Boolean;
    fsTotalDia: Double;
    fsDescricao: String;
 public
    property Aliquota  : Double  read fsAliquota write fsAliquota ;
    property Descricao : String  read fsDescricao write fsDescricao ;
    property Iss       : Boolean read fsISS      write fsISS ;
    property TotalDia  : Double  read fsTotalDia write fsTotalDia ;
end;

TACBrECFNaoFiscalFormaPagamento = class
 private
    fsDescricao: String;
    fsTotalDia: Double;
    fsIndice: String;
 public
    property Indice    : String read fsIndice    write fsIndice ;
    property Descricao : String read fsDescricao write fsDescricao ;
    property TotalDia  : Double  read fsTotalDia write fsTotalDia ;
end;

TACBrECFNaoFiscalComprovanteNaoFiscal = class
 private
    fsDescricao: String;
    fsTotalDia: Double;
    fsIndice: String;
 public
    property Indice    : String read fsIndice    write fsIndice ;
    property Descricao : String read fsDescricao write fsDescricao ;
    property TotalDia  : Double  read fsTotalDia write fsTotalDia ;
end;

TACBrECFNaoFiscalItemCupom = class
 private
    fsDescricao: String;
    fsValorUnit: Double;
    fsQtd: Double;
    fsPosAliq: Integer;
    fsCodigo: String;
    fsBaseICMS: Double;
    fsDesconto:Double;
 public
    property Codigo    : String  read fsCodigo    write fsCodigo    ;
    property Descricao : String  read fsDescricao write fsDescricao ;
    property Qtd       : Double  read fsQtd       write fsQtd       ;
      { Se Qtd = 0 Item foi cancelado }
    property ValorUnit : Double  read fsValorUnit write fsValorUnit ;
    property PosAliq   : Integer read fsPosAliq   write fsPosAliq   ;
    property BaseICMS  : Double  read fsBaseICMS  write fsBaseICMS  ;
    property Desconto  : Double  read fsDesconto  Write fsDesconto  ;
end;

TACBrECFNaoFiscalPagamentoCupom = class
 private
    fsValorPago: Double;
    fsPosFPG: Integer;
 public
    property PosFPG    : Integer read fsPosFPG    write fsPosFPG   ;
    property ValorPago : Double  read fsValorPago write fsValorPago;
end;

TACBrECFNaoFiscalCNFCupom = class
 private
    fsValor  : Double;
    fsPosCNF : Integer;
 public
    property PosCNF : Integer read fsPosCNF write fsPosCNF   ;
    property Valor  : Double  read fsValor  write fsValor;
end;

{ Classe filha de TACBrECFClass com implementaçao para NaoFiscal }

{ TACBrECFNaoFiscal }

TACBrECFNaoFiscal = class( TACBrECFClass )
 private
    fsNomeArqINI: String;
    fsArqBuf    : TextFile;
    fsBuffer    : TStringList ;
    fsArqINI    : String ;
    fsNumSerie  : String ;
    fsNumECF    : String ;
    fsIE        : String ;
    fsCNPJ      : String ;
    fsPAF       : String ;
    fsIM        : String ;
    fsCRO       : Integer ;
    fsCabecalho : TStringList ;
    fsCabecalhoItem : TStringList ;
    fsMascaraItem : String ;
    fsGavetaCmd : AnsiString ;
    fsCortaPapelCompletoCmd  : AnsiString ;
    fsCortaPapelParcialCmd  : AnsiString ;
    fsVERAO     : Boolean ;
    fsDia       : TDateTime ;
    fsReducoesZ : Integer ;
    fsLeiturasX : Integer ;
    fsCuponsCancelados : Integer ;
    fsCuponsCanceladosTotal : Double;
    fsCOOInicial : Integer ;
    fsCOOFinal   : Integer ;
    fsNumCupom   : Integer ;
    fsNumGNF     : Integer ;
    fsNumGRG     : Integer ;
    fsNumCDC     : Integer ;
    fsNumCER     : Integer ;
    fsGrandeTotal : Double ;
    fsVendaBruta  : Double ;
    fsNumCCF     : Integer ;
    fsSubTotal   : Double ;
    fsTotalPago  : Double ;

    fsEXEName : String ;

    fsAliquotas              : TObjectList ;
    fsFormasPagamento        : TObjectList ;
    fsComprovantesNaoFiscais : TObjectList ;
    fsItensCupom             : TObjectList ;
    fsItensCount             : Integer ;
    fsPagamentosCupom        : TObjectList ;
    fsCNFCupom               : TObjectList ;

    fswVERAO     : Boolean ;
    fswDia       : TDateTime ;
    fswESTADO    : TACBrECFEstado ;
    fswReducoesZ : Integer ;
    fswLeiturasX : Integer ;
    fswCuponsCancelados : Integer ;
    fswCuponsCanceladosTotal : Double;
    fswCOOInicial : Integer ;
    fswCOOFinal   : Integer ;
    fswNumCupom   : Integer ;
    fswNumGNF     : Integer ;
    fswNumGRG     : Integer ;
    fswNumCDC     : Integer ;
    fswNumCER     : Integer ;
    fswGrandeTotal  : Double ;
    fswVendaBruta   : Double ;
    fswNumCCF     : Integer ;
    fswSubTotal   : Double ;
    fswTotalPago  : Double ;

    fswAliquotas              : TObjectList ;
    fswFormasPagamento        : TObjectList ;
    fswComprovantesNaoFiscais : TObjectList ;
    fswItensCupom             : TObjectList ;
    fswPagamentosCupom        : TObjectList ;

    fsCmdImpCondensado: AnsiString;
    fsCmdImpExpandidoUmaLinha: AnsiString;
    fsCmdImpFimExpandido: AnsiString;
    fsCmdImpZera: AnsiString;

    function NumeroColunas: Integer;

    Procedure CopyAliquotas( FromObjectList, ToObjectList : TObjectList ) ;
    Procedure CopyFormasPagamento( FromObjectList, ToObjectList : TObjectList );
    Procedure CopyComprovantesNaoFiscais( FromObjectList, ToObjectList :
       TObjectList );
    Procedure CopyItensCupom( FromObjectList, ToObjectList : TObjectList );
    Procedure CopyPagamentosCupom( FromObjectList, ToObjectList : TObjectList );

    Procedure SalvaEstadoAtual ;
    Procedure RestauraEstadoAnterior ;
    Procedure AvisoLegal ;
    procedure LeArqINI ;
    procedure GravaArqINI ;
    procedure SetNomeArqINI(const Value: String);
    procedure AbreBuffer ;
    procedure GravaBuffer ;
    procedure ZeraBuffer ;
    procedure ImprimeBuffer ;
    procedure ImprimePorta( AString : AnsiString ) ; overload ;
    procedure ImprimePorta( AStringList : TStringList ) ; overload ;
    Procedure AddBufferRelatorio ;
    Procedure AddBufferRodape ;
    Procedure InsertCabecalho( AStringList : TStringList ) ;
    Procedure AddBufferCabecalho_Item ;
    Procedure ListaLeituraX( Est : TACBrECFEstado );
    Procedure AbreDocumento(AbreDia : Boolean = false) ;

    function AchaFPGIndiceNaoFiscal( const Indice: String) : Integer ;
    function AchaCNFIndiceNaoFiscal( const Indice: String) : Integer ;
    function GetNomeArqINI: String;

    function CalcTotalItem( AQtd, APrecoUnit: Double): Double;

 protected
    function GetNumCupom: String; override ;
    function GetNumGNF: String; override ;
    function GetNumGRG: String; override ;
    function GetNumCDC: String; override ;
    function GetGrandeTotal: Double; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;

    function GetNumCCF: String; override ;
    function GetNumECF: String; override ;
    function GetIE: String; override ;
    function GetCNPJ: String; override ;
    function GetPAF: String; override ;
    function GetUsuarioAtual: String; override ;
    function GetIM: String; override ;
    function GetCliche: AnsiString ; override ;
    function GetDataHoraSB: TDateTime; override ;
    function GetSubModeloECF: String ; override ;
    function GetNumCRO: String; override ;
    function GetNumSerie: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;

    function GetNumUltimoItem: Integer; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetHorarioVerao: Boolean; override ;
    function GetArredonda : Boolean; override ;

    function GetDataMovimento: TDateTime; override;
 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    { Propriedades Publicas de ACBrECFNaoFiscal }
    property CmdImpCondensado : AnsiString read fsCmdImpCondensado
       write fsCmdImpCondensado ;
    property CmdImpExpandidoUmaLinha : AnsiString read fsCmdImpExpandidoUmaLinha
       write fsCmdImpExpandidoUmaLinha ;
    property CmdImpFimExpandido : AnsiString read fsCmdImpFimExpandido
       write fsCmdImpFimExpandido ;
    property CmdImpZera : AnsiString read fsCmdImpZera write fsCmdImpZera ;
    property GavetaCmd  : AnsiString read fsGavetaCmd write fsGavetaCmd ;
    property CortaPapelCompletoCmd  : AnsiString read fsCortaPapelCompletoCmd
       write fsCortaPapelCompletoCmd ;
    property CortaPapelParcialCmd  : AnsiString read fsCortaPapelParcialCmd
       write fsCortaPapelParcialCmd ;
    property NomeArqINI : String read GetNomeArqINI write SetNomeArqINI ;

    procedure Ativar ; override ;
    procedure Desativar ; override ;
    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '' ) ; override ;
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;

    Procedure LeituraX ; override ;
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;
    Procedure CortaPapel( const CorteParcial : Boolean = false) ; override ;

    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;

    Procedure AbreGaveta ; override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota ; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    function AchaICMSAliquota( Aliquota : Double; Tipo : Char = ' ' ) :
       TACBrECFAliquota ; overload ; override;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  overload ; override ;

    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    function AchaFPGDescricao( Descricao : String;
       BuscaExata : Boolean = False;
       IgnorarCase : Boolean = True;
       IgnorarAcentos : Boolean = False) : TACBrECFFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    function AchaCNFDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True  ) :
       TACBrECFComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;
 end ;

Function StuffMascaraItem( const Linha, MascaraItem : AnsiString; Letra : AnsiChar;
       const TextoInserir : AnsiString; Fim:Boolean = False) : AnsiString ;

implementation
Uses ACBrUtil;

Function StuffMascaraItem( const Linha, MascaraItem : AnsiString; Letra : AnsiChar;
   const TextoInserir : AnsiString; Fim:Boolean = False) : AnsiString ;
Var A,B : Integer ;
    L   : AnsiChar ;
begin
  Result := '' ;

  if not FIM then
   begin
     B := 1 ;
     For A := 1 to length(MascaraItem) do
     begin
        L := MascaraItem[A] ;

        if L = Letra then
           if B > Length( TextoInserir ) then
              L := ' '
           else
            begin
              L := TextoInserir[B] ;
              B := B + 1 ;
            end
        else
           if A > Length( Linha ) then
              L := ' '
           else
              L := Linha[A] ;

        Result := Result + L ;
     end
   end
  else
   begin
     B := length(TextoInserir) ;
     For A := length(MascaraItem) downto 1 do
     begin
        L := MascaraItem[A] ;

        if L = Letra then
           if B < 1 then
              L := ' '
           else
            begin
              L := TextoInserir[B] ;
              B := B - 1 ;
            end
        else
           if A > Length( Linha ) then
              L := ' '
           else
              L := Linha[A] ;

        Result := L + Result ;
     end
   end;
end;

{ ----------------------------- TDJECFNaoFiscal ------------------------------ }

constructor TACBrECFNaoFiscal.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.HandShake := hsRTS_CTS ;

  { Variaveis internas dessa classe }
  fsArqINI    := '' ;
  fsNumSerie  := '' ;
  fsNumECF    := '' ;
  fsIE        := '012.345.678.90' ;
  fsCNPJ      := '01.234.567/0001-22' ;
  fsPAF       := 'ACBrMonitor' ;
  fsIM        := '1234-0' ;
  fsCRO       := 1 ;
  fpModeloStr := 'NaoFiscal' ;
  fsBuffer    := TStringList.create ;
  fsEXEName   := ParamStr(0) ;

  fsCabecalho := TStringList.create ;
  fsCabecalho.add('Nome da Empresa') ;
  fsCabecalho.add('Nome da Rua , 1234  -  Bairro') ;
  fsCabecalho.add('Cidade  -  UF  -  99999-999') ;
  fsCabecalho.add('CNPJ: 01.234.567/0001-22    IE: 012.345.678.90') ;

  fsCabecalhoItem := TStringList.create ;
  fsCabecalhoItem.Add('ITEM   CODIGO             DESCRICAO') ;
  fsCabecalhoItem.Add('.             QTDxUNITARIO   Aliq    VALOR (R$)') ;
  fsCabecalhoItem.Add( StringOfChar('-',Colunas) ) ;
  
  fsMascaraItem := 'III CCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD '+
                   'QQQQQQQQ UUxVVVVVVVVV AAAAAAA TTTTTTTTTTT' ;
  fsGavetaCmd := '' ;
  fsCortaPapelParcialCmd  := '';
  fsCortaPapelCompletoCmd := '';
  fsVERAO     := false ;
  fsDia       := now ;
  fsReducoesZ := 0 ;
  fsLeiturasX := 0;
  fsCOOInicial:= 0 ;
  fsCOOFinal  := 0 ;
  fsNumCupom  := 0 ;
  fsNumGNF    := 0 ;
  fsNumGRG    := 0 ;
  fsNumCDC    := 0 ;
  fsNumCER    := 0 ;
  fsGrandeTotal    := 0 ;
  fsVendaBruta     := 0 ;
  fsNumCCF    := 0 ;
  fsSubTotal  := 0 ;
  fsTotalPago := 0 ;
  fsCuponsCancelados := 0 ;
  fsCuponsCanceladosTotal := 0;

  fsCmdImpCondensado        := cCmdImpCondensado ;
  fsCmdImpExpandidoUmaLinha := cCmdImpExpandidoUmaLinha ;
  fsCmdImpFimExpandido      := cCmdImpFimExpandido;
  fsCmdImpZera              := cCmdImpZera ;

  fsAliquotas               := TObjectList.Create( true );
  fsFormasPagamento         := TObjectList.Create( true );
  fsComprovantesNaoFiscais  := TObjectList.Create( true );
  fsItensCupom              := TObjectList.Create( true );
  fsItensCount              := 0 ;
  fsPagamentosCupom         := TObjectList.Create( true );
  fsCNFCupom                := TObjectList.Create( true );

  fswAliquotas              := TObjectList.Create( true );
  fswFormasPagamento        := TObjectList.Create( true );
  fswComprovantesNaoFiscais := TObjectList.Create( true );
  fswItensCupom             := TObjectList.Create( true );
  fswPagamentosCupom        := TObjectList.Create( true );

  fpArredondaItemMFD := True;

  SalvaEstadoAtual ;
end;

destructor TACBrECFNaoFiscal.Destroy;
begin
  fsCabecalho.Free ;
  fsCabecalhoItem.Free ;

  fsAliquotas.Free ;
  fsFormasPagamento.Free ;
  fsComprovantesNaoFiscais.Free ;
  fsItensCupom.Free ;
  fsPagamentosCupom.Free ;
  fsCNFCupom.Free ;

  fswAliquotas.Free ;
  fswFormasPagamento.Free ;
  fswComprovantesNaoFiscais.Free ;
  fswItensCupom.Free ;
  fswPagamentosCupom.Free ;

  fsBuffer.Free ;
  Desativar ;

  inherited Destroy ;
  IOResult;
end;

procedure TACBrECFNaoFiscal.Ativar;
Var NumECF : Integer ;
begin
  inherited Ativar ; { Abre porta serial / paralela ou arquivo }

  fsNumSerie := '' ;
  fsNumECF   := '' ;
  fsCRO      := 1 ;
  fpIdentificaConsumidorRodape := True ;

  try
     if fsNomeArqINI <> '' then
        fsArqINI := fsNomeArqINI
     else
      begin
        NumECF   := Max(fpDevice.Tag, 1) ;
        fsNumECF := IntToStrZero( NumECF, 3 ) ;
        fsArqINI := ExtractFilePath(fsEXEName)+'ACBrECF'+fsNumECF+'.INI';
      end ;

     LeArqINI ;

     if fpDevice.IsSerialPort then
        fpDevice.Serial.Purge ;

     if not EmLinha() then
        raise EACBrECFERRO.Create(ACBrStr('Impressora: '+fpModeloStr+' não está pronta.')) ;

     AbreBuffer ;

     ImprimePorta( CmdImpZera + CmdImpCondensado );

     if fsBuffer.Count > 0 then
        ImprimeBuffer ;
  except
     Desativar ;
     raise ;
  end ;
end;

procedure TACBrECFNaoFiscal.Desativar;
begin
  {$I-}
  try
     CloseFile( fsArqBuf ) ;
  except
  end ;
  {$I+}

  inherited Desativar ;
end;

function TACBrECFNaoFiscal.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
begin
  cmd := AjustaLinhas(cmd, Colunas) ;
  ImprimePorta( cmd );

  fpComandoEnviado := cmd ;
  Result           := cmd ;
end;

function TACBrECFNaoFiscal.GetNumCupom: String;
begin
  Result := IntToStrZero( fsNumCupom, 6 ) ;
end;

function TACBrECFNaoFiscal.GetNumGNF: String;
begin
  Result := IntToStrZero( fsNumGNF, 6 ) ;
end;

function TACBrECFNaoFiscal.GetNumGRG: String;
begin
  Result := IntToStrZero( fsNumGRG, 6 ) ;
end;

function TACBrECFNaoFiscal.GetNumCDC: String;
begin
  Result := IntToStrZero( fsNumCDC, 6 ) ;
end;

function TACBrECFNaoFiscal.GetGrandeTotal: Double;
begin
  Result := RoundTo( fsGrandeTotal,-2) ;
end;

function TACBrECFNaoFiscal.GetVendaBruta: Double;
begin
  Result := RoundTo( fsVendaBruta,-2) ;
end;

function TACBrECFNaoFiscal.GetTotalSubstituicaoTributaria: Double;
begin
  Result := RoundTo( TACBrECFNaoFiscalAliquota(fsAliquotas[0]).TotalDia,-2 ) ;
end;

function TACBrECFNaoFiscal.GetTotalNaoTributado: Double;
begin
  Result := RoundTo( TACBrECFNaoFiscalAliquota(fsAliquotas[1]).TotalDia,-2 ) ;
end;

function TACBrECFNaoFiscal.GetTotalIsencao: Double;
begin
  Result := RoundTo( TACBrECFNaoFiscalAliquota(fsAliquotas[2]).TotalDia,-2 ) ;
end;

function TACBrECFNaoFiscal.GetNumCCF: String;
begin
  Result := IntToStrZero( fsNumCCF, 6 ) ;
end;

function TACBrECFNaoFiscal.GetNumECF: String;
begin
  Result := fsNumECF ;
end;

function TACBrECFNaoFiscal.GetIE: String;
begin
  Result := fsIE ;
end;

function TACBrECFNaoFiscal.GetCNPJ: String;
begin
  Result := fsCNPJ ;
end;

function TACBrECFNaoFiscal.GetPAF: String;
begin
  Result := fsPAF ;
end;

function TACBrECFNaoFiscal.GetUsuarioAtual: String;
begin
  Result := '0001' ;
end;

function TACBrECFNaoFiscal.GetIM: String;
begin
  Result := fsIM ;
end;

function TACBrECFNaoFiscal.GetCliche: AnsiString ;
Var A : Integer ;
begin
  Result := '' ;
  For A := 0 to fsCabecalho.Count - 1 do
  begin
  Result := Result +
            PadCenter(fsCabecalho[A], Colunas) +
            AnsiChar( chr(13)) + AnsiChar( chr(10)) ;
  end ;
end;

function TACBrECFNaoFiscal.GetDataHoraSB: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
begin
  RetCmd := '19072004080700' ;
  OldShortDateFormat := ShortDateFormat ;
  try
     ShortDateFormat := 'dd/mm/yyyy' ;
     result := StrToDate(copy(RetCmd, 1,2) + DateSeparator +
                         copy(RetCmd, 3,2) + DateSeparator +
                         copy(RetCmd, 5,4)) ;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;
  result := RecodeHour(  result,StrToInt(copy(RetCmd, 9,2))) ;
  result := RecodeMinute(result,StrToInt(copy(RetCmd,11,2))) ;
  result := RecodeSecond(result,StrToInt(copy(RetCmd,13,2))) ;
end;

function TACBrECFNaoFiscal.GetSubModeloECF: String;
begin
  Result := 'BETA' ;
end;

function TACBrECFNaoFiscal.GetNumSerie: String;
begin
  Result := fsNumSerie ;
end;

function TACBrECFNaoFiscal.GetNumUltimoItem: Integer;
begin
  Result := fsItensCupom.Count;
end;

function TACBrECFNaoFiscal.GetNumCRO: String;
begin
  Result := IntToStrZero(fsCRO,3) ;
end;

function TACBrECFNaoFiscal.GetNumVersao: String ;
begin
  Result := ACBR_VERSAO ;
end;

function TACBrECFNaoFiscal.GetTotalPago: Double;
begin
  Result := RoundTo( fsTotalPago,-2) ;
end;

function TACBrECFNaoFiscal.GetSubTotal: Double;
begin
  Result := RoundTo( fsSubTotal,-2) ;
end;

procedure TACBrECFNaoFiscal.ListaLeituraX( Est : TACBrECFEstado );
var
    A: Integer ;
    AbreDia : Boolean ;
begin
  if Estado <> estRequerX then
     TestaPodeAbrirCupom ;

  ZeraBuffer ;

  fsBuffer.Add( fsCmdImpExpandidoUmaLinha + PadCenter('LEITURA X', NumeroColunas() ) + fsCmdImpFimExpandido );

  AddBufferRelatorio ;

  if Est = estRelatorio then
     For A := 1 to LinhasEntreCupons do
        fsBuffer.Delete(fsBuffer.Count-1) ;

  SalvaEstadoAtual ;

  AbreDia := ( Estado in [estRequerX, estRequerZ] ) ;
  fsItensCupom.Clear ;
  fsPagamentosCupom.Clear ;
  fsCNFCupom.Clear ;
  fsLeiturasX := fsLeiturasX + 1 ;
  fsTotalPago := 0 ;
  fsSubTotal  := 0 ;
  fpESTADO    := Est ;

  try
     AbreDocumento( AbreDia ) ;
  except
     RestauraEstadoAnterior ;
     raise ;
  end ;
end;

procedure TACBrECFNaoFiscal.LeituraX ;
begin
  ListaLeituraX( estLivre );
end;

procedure TACBrECFNaoFiscal.AbreGaveta ;
var
  Ini : TIniFile;
begin
  if fsGavetaCmd = '' then
  begin
     Ini := TIniFile.Create( fsArqINI ); //quando for vazio le o INI novamente;
     try
        fsGavetaCmd :=  AscToString( Ini.ReadString('Impressora',
                                    'Comando_Abrir_Gaveta', #027+'v'+#150) );
     finally
       Ini.Free;
     end;
  end ;

  ImprimePorta( fsGavetaCmd );
end;

procedure TACBrECFNaoFiscal.ReducaoZ(DataHora : TDateTime) ;
var
  A: Integer ;
begin
  if Estado = estBloqueada then
     raise EACBrECFERRO.Create(ACBrStr('Dia já foi fechado. Redução Z já emitida')) ;

  if not (Estado in [estLivre,estRequerZ]) then
     raise EACBrECFERRO.create(ACBrStr('O Estado da Impressora não é "LIVRE" Cancele o '+
                            'último Documento')) ;

  ZeraBuffer ;

  fsBuffer.Add( fsCmdImpExpandidoUmaLinha + PadCenter('REDUCAO Z', NumeroColunas() ) + fsCmdImpFimExpandido );

  AddBufferRelatorio ;

  SalvaEstadoAtual ;

  fsItensCupom.Clear ;
  fsPagamentosCupom.Clear ;
  fsCNFCupom.Clear ;
  fsReducoesZ := fsReducoesZ + 1 ;
  if fpEstado = estRequerZ then
   begin
     fpEstado := estLivre ;
     fsDia    := now ;
   end
  else
     fpEstado := estBloqueada ;

  fsTotalPago := 0 ;
  fsSubTotal  := 0 ;
  fsCuponsCancelados := 0 ;
  fsCuponsCanceladosTotal := 0;
  fsVendaBruta := 0 ;
  fsNumCER := 0 ;

  For A := 0 to fsAliquotas.Count - 1 do
     TACBrECFNaoFiscalAliquota( fsAliquotas[A]).TotalDia := 0 ;

  For A := 0 to fsFormasPagamento.Count - 1 do
     TACBrECFNaoFiscalFormaPagamento( fsFormasPagamento[A]).TotalDia := 0 ;

  For A := 0 to fsComprovantesNaoFiscais.Count - 1 do
     TACBrECFNaoFiscalComprovanteNaoFiscal( fsComprovantesNaoFiscais[A]).
        TotalDia := 0 ;

  try
     AbreDocumento( true ) ;
  except
     RestauraEstadoAnterior ;
     raise ;
  end ;

end;

procedure TACBrECFNaoFiscal.MudaHorarioVerao ;
begin
  fsVERAO := not fsVERAO ;

  try
     GravaArqINI ;
  except
     fsVERAO := not fsVERAO ;
     raise ;
  end ;
end;

procedure TACBrECFNaoFiscal.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  if EHorarioVerao <> HorarioVerao then
     MudaHorarioVerao ;
end;

procedure TACBrECFNaoFiscal.AbreCupom ;
begin
  TestaPodeAbrirCupom ;

  ZeraBuffer ;
  if Consumidor.Documento <> '' then
  begin
     fsBuffer.Add( StringofChar('-',Colunas) ) ;
     if Consumidor.Documento <> '' then
        fsBuffer.Add(PadRight('CPF/CNPJ consumidor: '+Consumidor.Documento,Colunas)) ;

     if Consumidor.Nome <> '' then
        fsBuffer.Add(PadRight('Nome: '+Consumidor.Nome,Colunas)) ;

     if Consumidor.Endereco <> '' then
        fsBuffer.Add(PadRight('Endereco: '+Consumidor.Endereco,Colunas)) ;

     fsBuffer.Add( StringofChar('-',Colunas) ) ;
  end ;
  AddBufferCabecalho_Item ;

  SalvaEstadoAtual ;

  fsItensCupom.Clear ;
  fsPagamentosCupom.Clear ;
  fsCNFCupom.Clear ;
  fsTotalPago := 0 ;
  fsSubTotal  := 0 ;
  fpESTADO    := estVenda ;
  fsNumCCF    := fsNumCCF + 1 ;

  try
     AbreDocumento ;
//     Sleep(7000) ;   // Simulando um atraso //
     Consumidor.Enviado := True ;
  except
     RestauraEstadoAnterior ;
     raise ;
  end ;
end;

procedure TACBrECFNaoFiscal.CancelaCupom(NumCOOCancelar: Integer);
Var A : Integer ;
begin
  if ((fsItensCupom.Count = 0) and (Estado <> estVenda) ) and
     ((fsCNFCupom.Count   = 0) and (Estado <> estNaoFiscal) ) then
     raise EACBrECFERRO.Create(ACBrStr('Último Documento não é Cupom')) ;

  ZeraBuffer ;

  fsBuffer.Add( fsCmdImpExpandidoUmaLinha +
		PadCenter('*** CUPOM CANCELADO ***', NumeroColunas() ) + fsCmdImpFimExpandido ) ;

  SalvaEstadoAtual ;

  case Estado of
    estVenda :
       begin
          fsBuffer.Insert(0, fsCmdImpExpandidoUmaLinha +
                PadSpace('TOTAL  R$|'+FormatFloat('#,###,##0.00',Subtotal),
		     NumeroColunas(), '|') + fsCmdImpFimExpandido ) ;

          AddBufferRodape ;
       end ;

    estPagamento, estNaoFiscal :
       AddBufferRodape ;

  else
   begin
     fsNumCupom := fsNumCupom + 1 ;

     InsertCabecalho( fsBuffer );

     fsBuffer.Add( PadSpace('COO do Cupom Cancelado:|'+IntToStrZero(fsNumCupom-1,6),
                        Colunas,'|') ) ;
     fsBuffer.Add( PadSpace('Valor da Operacao  R$:|'+
                         FormatFloat('#,###,##0.00',Subtotal),Colunas,'|') );
     fsBuffer.Add( '' ) ;
     fsBuffer.Add( 'Nome do Consumidor:' ) ;
     fsBuffer.Add( '' ) ;
     fsBuffer.Add( 'Telefone do Cosumidor:' ) ;
     fsBuffer.Add( '' ) ;

     AddBufferRodape ;
   end ;
  end;

  fsCuponsCancelados := fsCuponsCancelados + 1 ;
  fsCuponsCanceladosTotal := fsCuponsCanceladosTotal + Subtotal;
  { Removendo do TotalDiario por Aliquotas }
  For A := 0 to fsItensCupom.Count - 1 do
     with TACBrECFNaoFiscalItemCupom( fsItensCupom[A] ) do
        with TACBrECFNaoFiscalAliquota( fsAliquotas[ PosAliq ] ) do
           TotalDia := RoundTo(TotalDia - BaseICMS,-2) ;

  { Removendo do TotalDiario por Pagamento }
  For A := 0 to fsPagamentosCupom.Count - 1 do
     with TACBrECFNaoFiscalPagamentoCupom( fsPagamentosCupom[A] ) do
        with TACBrECFNaoFiscalFormaPagamento( fsFormasPagamento[ PosFPG ] ) do
           TotalDia := RoundTo(TotalDia - ValorPago,-2) ;

  { Removendo do TotalDiario por CNF }
  For A := 0 to fsCNFCupom.Count - 1 do
     with TACBrECFNaoFiscalCNFCupom( fsCNFCupom[A] ) do
        with TACBrECFNaoFiscalComprovanteNaoFiscal( fsComprovantesNaoFiscais[ PosCNF ] ) do
           TotalDia := RoundTo(TotalDia - Valor,-2) ;

  fsItensCupom.Clear ;
  fsPagamentosCupom.Clear ;
  fsCNFCupom.Clear ;
  fpEstado   := estLivre ;
  fsSubTotal := 0 ;

  try
     GravaBuffer ;
     try
        GravaArqINI ;
        ImprimeBuffer
     finally
        ZeraBuffer ;
     end ;
  except
     RestauraEstadoAnterior ;
     raise ;
  end ;

end;

procedure TACBrECFNaoFiscal.CancelaItemVendido(NumItem: Integer);
Var Ini : TIniFile ;
begin
  if Estado <> estVenda then
     raise EACBrECFERRO.create(ACBrStr('O Estado da Impressora nao é "VENDA"')) ;

  if fsItensCupom.Count = 0 then
     raise EACBrECFERRO.create(ACBrStr('Nenhum Item foi vendido ainda')) ;

  if (NumItem < 1) or (NumItem > fsItensCupom.Count) then
     raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') fora da Faixa.')) ;

  if TACBrECFNaoFiscalItemCupom( fsItensCupom[NumItem-1] ).Qtd = 0 then
     raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+
                            ') já foi cancelado.')) ;

  ZeraBuffer ;
  fsBuffer.Add( 'CANCELADO ITEM: '+IntToStrZero( NumItem,3) ) ;

  ImprimeBuffer ;
  SalvaEstadoAtual ;

  try
     with TACBrECFNaoFiscalItemCupom( fsItensCupom[NumItem-1] ) do
     begin
        fsSubTotal := RoundTo(Subtotal - ( CalcTotalItem(Qtd, ValorUnit) + Desconto ) ,-2);
        Qtd := 0;

        with TACBrECFNaoFiscalAliquota( fsAliquotas[PosAliq] ) do
        begin
           TotalDia := max( RoundTo(TotalDia - BaseICMS,-2),0) ;
           BaseICMS := 0 ;

           { Grava apenas os dados do Item alterado, para não sobrecarregar o
             sistema, gravando todo o INI a cada Item Cancelado }
           Ini := TIniFile.Create( fsArqINI ) ;
           try
              Ini.WriteFloat( 'Item_Cupom'+IntToStrZero( NumItem-1, 3) ,
                              'Qtd', Qtd ) ;

              Ini.WriteFloat('Variaveis','SubTotal',SubTotal) ;

              Ini.WriteFloat( 'Aliquota'+IntToStrZero( PosAliq, 2) ,'TotalDia',
                              TotalDia ) ;
           finally
              Ini.Free ;
           end ;
        end ;
     end ;
  except
     RestauraEstadoAnterior ;
  end ;

end;

procedure TACBrECFNaoFiscal.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
Var FPG    : TACBrECFNaoFiscalFormaPagamento ;
    PosFPG : Integer ;
    Troco  : Double ;
    UltLin : String ;
    Pagto  : TACBrECFNaoFiscalPagamentoCupom ;
begin
  if Estado <> estPagamento then
     raise EACBrECFERRO.create(ACBrStr('O Estado da Impressora nao é "PAGAMENTO"')) ;

  if TotalPago >= SubTotal then
     raise EACBrECFERRO.create(ACBrStr('Total pago já foi atingido Cupom deve ser '+
                            'encerrado')) ;

  PosFPG := AchaFPGIndiceNaoFiscal( CodFormaPagto ) ;
  try
     if PosFPG < 0 then
        raise EACBrECFERRO.create('')  ;

     FPG := TACBrECFNaoFiscalFormaPagamento( fsFormasPagamento[ PosFPG ] ) ;
  except
     raise EACBrECFERRO.create(ACBrStr('Forma de Pagamento '+CodFormaPagto+' Inválida')) ;
  end ;

  Troco := 0 ;
  ZeraBuffer ;
  fsBuffer.Add( PadSpace(FPG.Descricao+'|'+
                FormatFloat('#,###,##0.00',Valor),Colunas,'|') ) ;

  while Observacao <> '' do
  begin
     fsBuffer.Add( copy(Observacao,1,Colunas) ) ;
     Observacao := copy(Observacao,Colunas + 1,length(Observacao) ) ;
  end ;

  if RoundTo(TotalPago + Valor,-2) >= SubTotal then   { Ultrapassou o Valor do Cupom }
  begin
     if fsPagamentosCupom.Count > 0 then
        UltLin := PadSpace('SOMA  R$|'+FormatFloat('#,###,##0.00',
                    RoundTo(TotalPago + Valor,-2)),Colunas,'|')
     else
        UltLin := '' ;

     if RoundTo(TotalPago + Valor,-2) > SubTotal then  { Tem TROCO ? }
     begin
        if UltLin <> '' then
           fsBuffer.Add( UltLin ) ;

        Troco  := RoundTo((TotalPago + Valor) - SubTotal,-2) ;
        UltLin :=  fsCmdImpExpandidoUmaLinha +
                   PadSpace('TROCO  R$|'+FormatFloat('#,###,##0.00',Troco),
		     NumeroColunas() ,'|' ) + fsCmdImpFimExpandido ;
     end ;

    // UltLin := StringReplace( UltLin,' ','_',[rfReplaceAll]) ;
     if UltLin <> '' then
        fsBuffer.Add( UltLin ) ;
  end ;

  ImprimeBuffer ;
  SalvaEstadoAtual ;

  try
     fsTotalPago  := RoundTo(TotalPago  + Valor,-2) ;
     FPG.TotalDia := RoundTo(FPG.TotalDia + Valor,-2) ;

     Pagto := TACBrECFNaoFiscalPagamentoCupom.Create ;
     Pagto.PosFPG    := PosFPG ;
     Pagto.ValorPago := Valor ;
     fsPagamentosCupom.Add( Pagto ) ;

     { Se tiver Troco, remove de 01 - DINHEIRO (indice = 0) }
     if Troco > 0 then
     begin
        FPG := TACBrECFNaoFiscalFormaPagamento( fsFormasPagamento[ 0 ] ) ;

        FPG.TotalDia := RoundTo(FPG.TotalDia - Troco,-2) ;

        { Lançando o Troco como um pagamento no Cupom, porém negativo, com isso
          o Cancelamento de Cupom conseguir desfaze-lo }
        Pagto := TACBrECFNaoFiscalPagamentoCupom.Create ;
        Pagto.PosFPG    := 0 ;
        Pagto.ValorPago := -Troco ;
        fsPagamentosCupom.Add( Pagto ) ;
     end ;

     GravaArqINI ;
  except
     RestauraEstadoAnterior ;
  end ;
end;

procedure TACBrECFNaoFiscal.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
var
  wEstado    : TACBrECFEstado ;
  wSubTotal  : Double;
  wTotalPago : Double;
begin
  if Estado <> estPagamento then
     raise EACBrECFERRO.create(ACBrStr('O Estado da Impressora nao é "PAGAMENTO"'+
                            'Não houve SubTotal')) ;

  if TotalPago < SubTotal then
     raise EACBrECFERRO.create(ACBrStr('Total Pago é inferior ao Total do Cupom')) ;

  Observacao := StringReplace(Observacao,#10,CRLF,[rfReplaceAll]) ;
  
  fsBuffer.Add( Observacao ) ;
  AddBufferRodape ;

  ImprimeBuffer ;
  wEstado    := Estado ;
  wSubTotal  := fsSubTotal;
  wTotalPago := fsTotalPago;
  Consumidor.Enviado := True ;

  try
     fpEstado    := estLivre ;
     //fsSubTotal  := 0.0;comentado para poder saber o valor do cancelado depois
     fsTotalPago := 0.0;

     GravaArqINI ;
  except
     fpEstado    := wEstado ;
     fsSubTotal  := wSubTotal;
     fsTotalPago := wTotalPago;
  end ;
end;

procedure TACBrECFNaoFiscal.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString );
Var Taxa, ValTaxa : Double ;
    S    : String ;
    A, AjusteSinal: Integer ;
begin
  if not (Estado in [estVenda, estNaoFiscal]) then
     raise EACBrECFERRO.create(ACBrStr('O Estado da Impressora nao é "VENDA"'+
                            ' Cupom não Aberto')) ;
  if SubTotal <= 0 then
     raise EACBrECFERRO.create(ACBrStr('Nenhum Item foi vendido ainda')) ;

  ZeraBuffer ;

  Taxa := 0 ;
  if DescontoAcrescimo <> 0 then
  begin
     Taxa := abs(DescontoAcrescimo) / SubTotal ;
     if DescontoAcrescimo < 0 then
        S := 'Desconto '
     else
        S := 'Acrescimo' ;
        
     fsBuffer.Add( PadSpace('SUBTOTAL   R$|'+
                   FormatFloat('#,###,##0.00',SubTotal), Colunas,'|') ) ;
     fsBuffer.Add( PadSpace(S+'  R$|'+FormatFloat('#,###,##0.00',DescontoAcrescimo),
                        Colunas,'|') ) ;
  end ;

  fsBuffer.Add(  fsCmdImpExpandidoUmaLinha +
                   PadSpace('TOTAL  R$|'+FormatFloat('#,###,##0.00',
                        SubTotal+DescontoAcrescimo),
			NumeroColunas() ,'|') + fsCmdImpFimExpandido ) ;
  ImprimeBuffer ;
  SalvaEstadoAtual ;

  if (DescontoAcrescimo < 0) then
    AjusteSinal := -1
  else
    AjusteSinal := 1;

  try
     if Taxa <> 0 then  { Aplicando DESCONTO/ACRECIMO em ALIQUOTAS por % }
     begin
        For A := 0 to fsItensCupom.Count - 1 do
        begin
           fsItensCount := -1 ;  { Força gravar os Itens, em GravaArqINI }

           with TACBrECFNaoFiscalItemCupom( fsItensCupom[A] ) do
           begin
	          ValTaxa := ( Qtd * ValorUnit * Taxa * AjusteSinal ) ;

              BaseICMS := RoundTo(BaseICMS + ValTaxa,-2) ;
              with TACBrECFNaoFiscalAliquota( fsAliquotas[ PosAliq ] ) do
              begin
                 TotalDia := RoundTo(TotalDia + ValTaxa,-2) ;
              end ;
           end ;
        end ;
     end ;

     fsSubTotal  := RoundTo( SubTotal + DescontoAcrescimo,-2) ;
     fpEstado    := estPagamento ;
     fsTotalPago := 0 ;

     GravaArqINI ;
  except
     RestauraEstadoAnterior ;
  end ;

end;

procedure TACBrECFNaoFiscal.VendeItem(Codigo, Descricao : String ;
   AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
   ValorDescontoAcrescimo : Double ; Unidade : String ;
   TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
   CodDepartamento : Integer) ;
Var PosAliq : Integer ;
    Aliq : TACBrECFNaoFiscalAliquota ;
    ItemCupom : TACBrECFNaoFiscalItemCupom ;
    StrQtd,StrPreco,Linha,StrDescAcre : String ;
    PorcDesc, ValDesc, Total : Double ;
    Ini  : TIniFile ;
begin
  if Estado <> estVenda then
     raise EACBrECFERRO.create(ACBrStr('O Estado da Impressora nao é "VENDA"'+
                            ' Cupom não Aberto')) ;

  if (Qtd <= 0) or (ValorUnitario <= 0) or (Descricao = '') or (Codigo = '') then
     raise EACBrECFERRO.create(ACBrStr('Erro. Parâmetros inválidos.')) ;

  try
     PosAliq := StrToInt(AliquotaECF)-1 ;
     Aliq    := TACBrECFNaoFiscalAliquota( fsAliquotas[ PosAliq ] ) ;
  except
     raise EACBrECFERRO.create(ACBrStr('Aliquota '+AliquotaECF+' Inválida')) ;
  end ;

  if PosAliq > 2 then
   begin
     if Aliq.Iss Then AliquotaECF := 'S' else AliquotaECF := 'T' ;
     AliquotaECF := PadCenter(AliquotaECF + FormatFloat('#0.00',Aliq.Aliquota)+'%',7)
   end
  else
     AliquotaECF := PadCenter(Aliq.Descricao,7) ;

  if Qtd = Round( Qtd ) then
     StrQtd := FormatFloat('#######0',Qtd )
  else
     StrQtd := FormatFloat('###0.000',Qtd ) ;

  if RoundTo( ValorUnitario, -2 ) = ValorUnitario then
     StrPreco := FormatFloat('#####0.00',ValorUnitario )
  else
     StrPreco := FormatFloat('####0.000',ValorUnitario ) ;

  Total   := CalcTotalItem( Qtd, ValorUnitario) ;
  ValDesc := 0 ;
  PorcDesc:= 0 ;
  if ValorDescontoAcrescimo > 0 then
  begin
     if TipoDescontoAcrescimo = '%' then
      begin
        PorcDesc := -ValorDescontoAcrescimo ;
        ValDesc  := -RoundTo(Total * (ValorDescontoAcrescimo / 100), -3) ;
      end
     else
      begin
        PorcDesc := -RoundTo( (ValorDescontoAcrescimo / Total) * 100, -2) ;
        ValDesc  := -ValorDescontoAcrescimo ;
      end ;

     StrDescAcre := 'DESCONTO' ;
     if DescontoAcrescimo <> 'D' then  // default, DescontoAcrescimo = 'D'
     begin
        ValDesc     := -ValDesc ;
        PorcDesc    := -PorcDesc ;
        StrDescAcre := 'ACRESCIMO' ;
     end ;

     if Abs(PorcDesc) >= 100 then
        raise EACBrECFERRO.create(ACBrStr(StrDescAcre+' maior do que 99,99%'));
  end ;

  { Inserindo na String da fsMascaraItem }
  Linha := fsMascaraItem ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'I', IntToStrZero(fsItensCupom.Count+1,3)) ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'C', Codigo ) ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'D', Descricao ) ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'Q', StrQtd ) ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'U', Unidade ) ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'V', StrPreco ) ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'A', AliquotaECF ) ;
  Linha := StuffMascaraItem( Linha, fsMascaraItem, 'T', FormatFloat('###,##0.00', Total ), True ) ;

  { Quebrando a linha pela COLUNA }
  Linha := Trim( Linha ) ;
  while Linha <> '' do
  begin
     fsBuffer.Add( copy(Linha,1,Colunas) ) ;
     Linha := copy(Linha,Colunas + 1,length(Linha) ) ;
  end ;

  if ValorDescontoAcrescimo > 0 then
  begin
     Total := RoundTo(Total + ValDesc, -2) ;
     fsBuffer.Add( PadSpace('|'+StrDescAcre+'|'+FormatFloat('#0.00', PorcDesc)+'%|R$ '+
                       FormatFloat('##,##0.00',ValDesc)+'|'+
                       FormatFloat('###,##0.00',Total),Colunas,'|') ) ;
  end ;

//  Sleep(1000) ;   // Simulando um atraso //
  ImprimeBuffer ;
  SalvaEstadoAtual ;

  try
     { Adcionando o Item Vendido no ObjectList }
     ItemCupom := TACBrECFNaoFiscalItemCupom.Create ;
     ItemCupom.Codigo    := Codigo ;
     ItemCupom.Descricao := Descricao ;
     ItemCupom.Qtd       := Qtd ;
     ItemCupom.ValorUnit := ValorUnitario ;
     ItemCupom.PosAliq   := PosAliq ;
     ItemCupom.BaseICMS  := Total ;
     ItemCupom.Desconto  := ValDesc;
     fsItensCupom.Add( ItemCupom ) ;
     fsItensCount := fsItensCupom.Count ;

     fsGrandeTotal := RoundTo( (Qtd * ValorUnitario) + fsGrandeTotal,-2) ;
     fsVendaBruta  := RoundTo( (Qtd * ValorUnitario) + fsVendaBruta,-2) ;
     Aliq.TotalDia := RoundTo( Aliq.TotalDia + Total,-2) ; { Soma na aliquota }
     fsSubTotal    := RoundTo( SubTotal + Total,-2) ;      { Soma no Subtotal }

     { Grava apenas os dados do Item para não sobrecarregar o sistema, gravando
       todo o INI a cada Item vendido }
     Ini := TIniFile.Create( fsArqINI ) ;
     try
        Linha := 'Item_Cupom'+IntToStrZero( fsItensCupom.Count-1, 3) ;
        Ini.WriteString( Linha ,'Descricao', ItemCupom.Descricao );
        Ini.WriteString( Linha ,'Codigo', ItemCupom.Codigo ) ;
        Ini.WriteFloat( Linha ,'Qtd', ItemCupom.Qtd ) ;
        Ini.WriteFloat( Linha ,'ValorUnit', ItemCupom.ValorUnit ) ;
        Ini.WriteInteger( Linha ,'PosAliq', ItemCupom.PosAliq) ;
        Ini.WriteFloat( Linha ,'BaseICMS', ItemCupom.BaseICMS) ;
        Ini.WriteFloat( Linha ,'Desconto', ItemCupom.Desconto) ;

        Ini.WriteFloat('Variaveis','SubTotal',SubTotal) ;
        Ini.WriteFloat('Variaveis','GrandeTotal',fsGrandeTotal) ;
        Ini.WriteFloat('Variaveis','VendaBruta',fsVendaBruta) ;
        Ini.WriteFloat( 'Aliquota'+IntToStrZero( PosAliq, 2) ,'TotalDia',
                        Aliq.TotalDia ) ;
     finally
        Ini.Free ;
     end ;
  except
     RestauraEstadoAnterior ;
  end ;
end ;

procedure TACBrECFNaoFiscal.AbreRelatorioGerencial(Indice : Integer) ;
begin
  if not (Estado in [estLivre,estRequerZ,estRequerX])  then
     raise EACBrECFERRO.Create(ACBrStr('O Estado da Impressora não é "LIVRE"'));

  fsNumGNF := fsNumGNF + 1 ;
  fsNumGRG := fsNumGRG + 1 ;
  fsNumCER := fsNumCER + 1 ;
  ZeraBuffer ;
  fsBuffer.Add( PadCenter('GNF:'+IntToStrZero(fsNumGNF,6) +'         '+
                'GRG:'+IntToStrZero(fsNumGRG,6),Colunas) ) ;
  fsBuffer.Add( StringOfChar('-',Colunas) ) ;
  fsBuffer.Add( PadCenter('NAO E DOCUMENTO FISCAL',Colunas) ) ;

  fsBuffer.Add( fsCmdImpExpandidoUmaLinha +
		PadCenter('RELATORIO GERENCIAL', NumeroColunas() ) + fsCmdImpFimExpandido );
  fsBuffer.Add( '' ) ;

  SalvaEstadoAtual ;

  fsItensCupom.Clear ;
  fsPagamentosCupom.Clear ;
  fsCNFCupom.Clear ;
  fsTotalPago := 0 ;
  fsSubTotal  := 0 ;
  fpEstado    := estRelatorio ;

  try
     AbreDocumento ;
  except
     RestauraEstadoAnterior ;
     raise ;
  end ;
end;

procedure TACBrECFNaoFiscal.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  Linha := AjustaLinhas( Linha, Colunas );
  Linha := StringReplace( Linha, #10, sLineBreak, [rfReplaceAll] ) ;

  ImprimePorta(Linha);
end;

procedure TACBrECFNaoFiscal.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
Var FPG : TACBrECFNaoFiscalFormaPagamento ;
    A, PosFPG : Integer ;
    UsouPagamento : Boolean ;
begin

  if COO = '' then
     raise EACBrECFERRO.create(ACBrStr('COO inválido'));

  if Estado <> estLivre  then
     raise EACBrECFERRO.Create(ACBrStr('O Estado da Impressora não é "LIVRE"')) ;

  if fsPagamentosCupom.Count < 1 then
     raise EACBrECFERRO.Create(ACBrStr('Ultimo Documento não é Cupom')) ;

  COO := Poem_Zeros(COO,6) ;

  PosFPG := AchaFPGIndiceNaoFiscal( CodFormaPagto ) ;
  if PosFPG < 0 then
     raise EACBrECFERRO.Create(ACBrStr('Posição de Pagamento: '+CodFormaPagto+' inválida'));

  FPG := TACBrECFNaoFiscalFormaPagamento( fsFormasPagamento[PosFPG] ) ;

  UsouPagamento := false ;
  A := 0 ;
  while (not UsouPagamento) and (A < fsPagamentosCupom.Count) do
  begin
     UsouPagamento := (TACBrECFNaoFiscalPagamentoCupom(
                       fsPagamentosCupom[A]).PosFPG = PosFPG ) ;
     Inc( A ) ;
  end ;

  if not UsouPagamento then
     raise EACBrECFERRO.create(ACBrStr('Forma de Pagamento: '+FPG.Descricao+
                            ' não foi utilizada no Cupom anterior')) ;

  fsNumGNF := fsNumGNF + 1 ;
  fsNumCDC := fsNumCDC + 1 ;
  ZeraBuffer ;
  fsBuffer.Add( PadCenter('GNF:'+IntToStrZero(fsNumGNF,6) +'         '+
                'CDC:'+IntToStrZero(fsNumCDC,6),Colunas) ) ;
  fsBuffer.Add( '' ) ;
  fsBuffer.Add( PadCenter('COMPROVANTE NAO FISCAL VINCULADO',Colunas) ) ;
  fsBuffer.Add( '' ) ;
  fsBuffer.Add( PadSpace('COO do documento de compra:|' + COO, Colunas,'|') ) ;
  fsBuffer.Add( PadSpace('VALOR TOTAL DA COMPRA   R$:|'+
                FormatFloat('##,###,##0.00',fswSubTotal),Colunas,'|') ) ;
  fsBuffer.Add( PadSpace(PadRight(FPG.Descricao,23)+' R$:|'+
                FormatFloat('##,###,##0.00',Valor),Colunas,'|') ) ;
  fsBuffer.Add( '' ) ;

  SalvaEstadoAtual ;

  fsItensCupom.Clear ;
  fsPagamentosCupom.Clear ;
  fsCNFCupom.Clear ;
  fsTotalPago := 0 ;
  fsSubTotal  := 0 ;
  fpEstado    := estRelatorio ;

  try
     AbreDocumento ;
  except
     RestauraEstadoAnterior ;
     raise ;
  end ;
end;

procedure TACBrECFNaoFiscal.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha );
end;

procedure TACBrECFNaoFiscal.FechaRelatorio;
var wEstado : TACBrECFEstado ;
begin
  if Estado <> estRelatorio then exit ;

  ZeraBuffer ;
  AddBufferRodape ;

  ImprimeBuffer ;
  wEstado := Estado ;

  try
     fpEstado := estLivre ;

     GravaArqINI ;
  except
     fpEstado := wEstado ;
  end ;
end;

procedure TACBrECFNaoFiscal.CortaPapel(const CorteParcial : Boolean) ;
var
  Ini : TIniFile;
begin
  if (fsCortaPapelCompletoCmd = '') or (fsCortaPapelParcialCmd = '') then
  begin
     Ini := TIniFile.Create( fsArqINI ); //quando for vazio le o INI novamente;
     try
        fsCortaPapelCompletoCmd :=  AscToString( Ini.ReadString('Impressora',
                                    'Comando_Corta_Papel_Completo', #027+#119) );
        fsCortaPapelParcialCmd  :=  AscToString( Ini.ReadString('Impressora',
                                    'Comando_Corta_Papel_Parcial', #027+#109) );
     finally
       Ini.Free;
     end;
  end ;

  If CorteParcial Then
     ImprimePorta( fsCortaPapelParcialCmd )
  else
     ImprimePorta( fsCortaPapelCompletoCmd ) ;

  Sleep(100);
end ;

procedure TACBrECFNaoFiscal.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  TestaPodeAbrirCupom ;

  ZeraBuffer ;
  CPF_CNPJ := trim(CPF_CNPJ) ;
  if CPF_CNPJ <> '' then
  begin
     fsBuffer.Add( StringofChar('-',Colunas) ) ;
     fsBuffer.Add(PadRight('CPF/CNPJ Consumidor: '+CPF_CNPJ,Colunas)) ;
     if Nome <> '' then
        fsBuffer.Add(PadRight('Nome: '+Nome,Colunas)) ;
     if Endereco <> '' then
        fsBuffer.Add(PadRight('Endereco: '+Endereco,Colunas)) ;
     fsBuffer.Add( StringofChar('-',Colunas) ) ;
  end ;

  SalvaEstadoAtual ;

  fsItensCupom.Clear ;
  fsPagamentosCupom.Clear ;
  fsCNFCupom.Clear ;
  fsTotalPago := 0 ;
  fsSubTotal  := 0 ;
  fpESTADO    := estNaoFiscal ;

  try
     AbreDocumento ;
  except
     RestauraEstadoAnterior ;
     raise ;
  end ;
end;

procedure TACBrECFNaoFiscal.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString);
Var CNFCupom : TACBrECFNaoFiscalCNFCupom ;
    CNF      : TACBrECFNaoFiscalComprovanteNaoFiscal ;
    PosCNF   : Integer ;
begin
  if Estado <> estNaoFiscal then
     raise EACBrECFERRO.create(ACBrStr('Comprovante Não Fiscal não foi aberto')) ;

  if (Valor <= 0) then
     raise EACBrECFERRO.create(ACBrStr('Erro. Parâmetros inválidos.')) ;

  PosCNF := AchaCNFIndiceNaoFiscal( CodCNF ) ;
  try
     if PosCNF < 0 then
        raise EACBrECFERRO.create('')  ;

     CNF := TACBrECFNaoFiscalComprovanteNaoFiscal( fsComprovantesNaoFiscais[ PosCNF ] ) ;
  except
     raise EACBrECFERRO.create(ACBrStr('Comprovante Não Fiscal '+CodCNF+' Inválido')) ;
  end ;

  ZeraBuffer ;
  fsBuffer.Add( PadSpace( IntToStrZero(fsCNFCupom.Count + 1,3) +'|'+
                      CNF.Descricao +'|'+
                      FormatFloat('#,###,##0.00',Valor), Colunas, '|'  ) ) ;

  while Obs <> '' do
  begin
     fsBuffer.Add( copy(Obs,1,Colunas) ) ;
     Obs := copy(Obs,Colunas + 1,length(Obs) ) ;
  end ;

  ImprimeBuffer ;
  SalvaEstadoAtual ;

  try
     fsSubTotal   := RoundTo( Subtotal + Valor,-2) ;      { Soma no Subtotal }
     
     CNF.TotalDia := RoundTo(CNF.TotalDia + Valor,-2) ;

     CNFCupom := TACBrECFNaoFiscalCNFCupom.Create ;
     CNFCupom.PosCNF := PosCNF ;
     CNFCupom.Valor  := Valor ;
     fsCNFCupom.Add( CNFCupom ) ;

     GravaArqINI ;
  except
     RestauraEstadoAnterior ;
  end ;
end;

procedure TACBrECFNaoFiscal.GravaArqINI;
Var Ini : TIniFile ;
    A   : Integer ;
    S   : String ;
    Aviso : String ;
    PrimeiraVez : Boolean ;
begin

  PrimeiraVez := not FileExists(fsArqINI)  ;
  Ini := TIniFile.Create( fsArqINI ) ;

  try
     Aviso := Ini.ReadString('Variaveis','Aviso_Legal','SIM') ;

     Ini.WriteInteger('Variaveis','Estado',Integer( fpEstado) ) ;
     Ini.WriteString('Variaveis','Aviso_Legal',Aviso) ;
     Ini.WriteString('Variaveis','NumECF',fsNumECF) ;
     Ini.WriteString('Variaveis','IE',fsIE) ;
     Ini.WriteString('Variaveis','CNPJ',fsCNPJ) ;
     Ini.WriteString('Variaveis','PAF',fsPAF) ;
     Ini.WriteString('Variaveis','IM',fsIM) ;          
     Ini.WriteString('Variaveis','NumSerie',fsNumSerie) ;
     Ini.WriteInteger('Variaveis','CRO',fsCRO) ;
     Ini.WriteInteger('Variaveis','NumCupom',fsNumCupom) ;
     Ini.WriteInteger('Variaveis','NumGNF',fsNumGNF) ;
     Ini.WriteInteger('Variaveis','NumGRG',fsNumGRG) ;
     Ini.WriteInteger('Variaveis','NumCDC',fsNumCDC) ;
     Ini.WriteInteger('Variaveis','NumCER',fsNumCER) ;
     Ini.WriteFloat('Variaveis','GrandeTotal',fsGrandeTotal) ;
     Ini.WriteFloat('Variaveis','VendaBruta',fsVendaBruta) ;
     Ini.WriteInteger('Variaveis','NumCCF',fsNumCCF) ;
     Ini.WriteDate('Variaveis','DiaMovimento',fsDia) ;
     Ini.WriteBool('Variaveis','HorarioVerao',fsVERAO) ;
     Ini.WriteInteger('Variaveis','ReducoesZ',fsReducoesZ) ;
     Ini.WriteInteger('Variaveis','LeiturasX',fsLeiturasX) ;
     Ini.WriteInteger('Variaveis','COOInicial',fsCOOInicial) ;
     Ini.WriteInteger('Variaveis','COOFinal',fsCOOFinal) ;
     Ini.WriteFloat('Variaveis','SubTotal',SubTotal) ;
     Ini.WriteFloat('Variaveis','TotalPago',TotalPago) ;
     Ini.WriteInteger('Variaveis','CuponsCancelados',fsCuponsCancelados) ;
     Ini.WriteFloat('Variaveis', 'CuponsCanceladosTotal', fsCuponsCanceladosTotal);
     Ini.WriteString('Variaveis','Operador',Operador) ;

     if PrimeiraVez then
     begin
        Ini.EraseSection('Cabecalho') ;
        For A := 0 to fsCabecalho.Count - 1 do
           Ini.WriteString('Cabecalho','LIN'+IntToStrZero(A,3),fsCabecalho[A] );

        Ini.EraseSection('Cabecalho_Item') ;
        For A := 0 to fsCabecalhoItem.Count - 1 do
           Ini.WriteString('Cabecalho_Item','LIN'+IntToStrZero(A,3),
              fsCabecalhoItem[A] );
        Ini.WriteString('Cabecalho_Item','MascaraItem',fsMascaraItem) ;

        Ini.WriteInteger('Impressora','Colunas', fpColunas) ;
        Ini.WriteString('Impressora','Comando_Abrir_Gaveta',
           StringToAsc(fsGavetaCmd) ) ;
        Ini.WriteString('Impressora','Comando_Corta_Papel_Completo',
           StringToAsc(fsCortaPapelCompletoCmd) ) ;
        Ini.WriteString('Impressora','Comando_Corta_Papel_Parcial',
           StringToAsc(fsCortaPapelParcialCmd) ) ;
        Ini.WriteString('Impressora','Comando_Incializacao',
           StringToAsc(fsCmdImpZera) ) ;
        Ini.WriteString('Impressora','Comando_Ativar_Condensado',
           StringToAsc(fsCmdImpCondensado) );
        Ini.WriteString('Impressora','Comando_Expandido_uma_Linha',
           StringToAsc(fsCmdImpExpandidoUmaLinha) ) ;
        Ini.WriteString('Impressora','Comando_Fim_Expandido',
           StringToAsc(fsCmdImpFimExpandido) ) ;
     end ;

     if fsItensCount <> fsItensCupom.Count then
     begin
        A := 0 ;
        while true do
        begin
           S := 'Item_Cupom'+IntToStrZero( A, 3) ;

           if A <= fsItensCupom.Count - 1 then
              with fsItensCupom[A] as TACBrECFNaoFiscalItemCupom do
              begin
                 Ini.WriteString( S ,'Descricao', Descricao );
                 Ini.WriteString( S ,'Codigo', Codigo ) ;
                 Ini.WriteFloat( S ,'Qtd', Qtd ) ;
                 Ini.WriteFloat( S ,'ValorUnit', ValorUnit ) ;
                 Ini.WriteInteger( S ,'PosAliq', PosAliq) ;
                 Ini.WriteFloat( S ,'BaseICMS', BaseICMS) ;
                 Ini.WriteFloat( S ,'Desconto', Desconto) ;
              end
           else
              if Ini.SectionExists( S ) then
                 Ini.EraseSection( S )
              else
                 break ;

           A := A + 1 ;
        end ;

        fsItensCount := fsItensCupom.Count ;
     end ;

     A := 0 ;
     while true do
     begin
        S := 'Pagamento_Cupom'+IntToStrZero( A, 2) ;

        if A <= fsPagamentosCupom.Count - 1 then
           with fsPagamentosCupom[A] as TACBrECFNaoFiscalPagamentoCupom do
           begin
              Ini.WriteInteger( S ,'PosFPG', PosFPG ) ;
              Ini.WriteFloat( S ,'ValorPago', ValorPago ) ;
           end
        else
           if Ini.SectionExists( S ) then
              Ini.EraseSection( S )
           else
              break ;

        A := A + 1 ;
     end ;

     A := 0 ;
     while true do
     begin
        S := 'CNF_Cupom'+IntToStrZero( A, 2) ;

        if A <= fsCNFCupom.Count - 1 then
           with fsCNFCupom[A] as TACBrECFNaoFiscalCNFCupom do
           begin
              Ini.WriteInteger( S ,'PosCNF', PosCNF ) ;
              Ini.WriteFloat( S ,'Valor', Valor ) ;
           end
        else
           if Ini.SectionExists( S ) then
              Ini.EraseSection( S )
           else
              break ;

        A := A + 1 ;
     end ;

     A := 0 ;
     while true do
     begin
        S := 'Forma_Pagamento'+IntToStrZero( A, 2) ;

        if A <= fsFormasPagamento.Count - 1 then
           with fsFormasPagamento[A] as TACBrECFNaoFiscalFormaPagamento do
           begin
              Ini.WriteString( S ,'Indice', Indice ) ;
              Ini.WriteString( S ,'Descricao', Descricao ) ;
              Ini.WriteFloat( S ,'TotalDia', TotalDia ) ;
           end
        else
           if Ini.SectionExists( S ) then
              Ini.EraseSection( S )
           else
              break ;

        A := A + 1 ;
     end ;

     A := 0 ;
     while true do
     begin
        S := 'Comprovante_nao_Fiscal'+IntToStrZero( A, 2) ;

        if A <= fsComprovantesNaoFiscais.Count - 1 then
           with fsComprovantesNaoFiscais[A] as TACBrECFNaoFiscalComprovanteNaoFiscal do
           begin
              Ini.WriteString( S ,'Indice', Indice ) ;
              Ini.WriteString( S ,'Descricao', Descricao ) ;
              Ini.WriteFloat( S ,'TotalDia', TotalDia ) ;
           end
        else
           if Ini.SectionExists( S ) then
              Ini.EraseSection( S )
           else
              break ;

        A := A + 1 ;
     end ;

     A := 0 ;
     while true do
     begin
        S := 'Aliquota'+IntToStrZero( A, 2) ;

        if A <= fsAliquotas.Count - 1 then
           with fsAliquotas[A] as TACBrECFNaoFiscalAliquota do
           begin
              Ini.WriteString( S ,'Descricao', Descricao ) ;
              Ini.WriteFloat( S ,'Aliquota', Aliquota ) ;
              Ini.WriteBool( S ,'ISS', Iss ) ;
              Ini.WriteFloat( S ,'TotalDia', TotalDia ) ;
           end
        else
           if Ini.SectionExists( S ) then
              Ini.EraseSection( S )
           else
              break ;

        A := A + 1 ;
     end ;
  finally
     Ini.Free ;
  end ;

end;

procedure TACBrECFNaoFiscal.LeArqINI;
Var Ini : TIniFile ;
    A,B : Integer ;
    S,T : String ;
    NumSerie : String ;
    ItemCupom            : TACBrECFNaoFiscalItemCupom ;
    PagamentoCupom       : TACBrECFNaoFiscalPagamentoCupom ;
    CNFCupom             : TACBrECFNaoFiscalCNFCupom ;
    AliqICMS             : TACBrECFNaoFiscalAliquota ;
    FormaPagamento       : TACBrECFNaoFiscalFormaPagamento ;
    ComprovanteNaoFiscal : TACBrECFNaoFiscalComprovanteNaoFiscal ;
begin

  if not FileExists( fsArqINI ) then
  begin
     AvisoLegal ;

     try
        DateTimeToString(NumSerie, 'ddmmyyhhnnss', now ) ;
        fsNumSerie := 'ACBR01NF'+NumSerie+' ' ;
     except
     end ;

     AliqICMS := TACBrECFNaoFiscalAliquota.create ;
     AliqICMS.Aliquota := 0 ;
     AliqICMS.Descricao:= 'FF' ;
     AliqICMS.Iss      := false ;
     AliqICMS.TotalDia := 0 ;
     fsAliquotas.Add( AliqICMS ) ;

     AliqICMS := TACBrECFNaoFiscalAliquota.create ;
     AliqICMS.Aliquota := 0 ;
     AliqICMS.Descricao:= 'NN' ;
     AliqICMS.Iss      := false ;
     AliqICMS.TotalDia := 0 ;
     fsAliquotas.Add( AliqICMS ) ;

     AliqICMS := TACBrECFNaoFiscalAliquota.create ;
     AliqICMS.Aliquota := 0 ;
     AliqICMS.Descricao:= 'II' ;
     AliqICMS.Iss      := false ;
     AliqICMS.TotalDia := 0 ;
     fsAliquotas.Add( AliqICMS ) ;
     FreeAndNil( fpAliquotas ) ; 

     FormaPagamento := TACBrECFNaoFiscalFormaPagamento.create ;
     FormaPagamento.Indice    := '01' ;
     FormaPagamento.Descricao := 'DINHEIRO' ;
     FormaPagamento.TotalDia  := 0 ;
     fsFormasPagamento.Add( FormaPagamento ) ;
     FreeAndNil( fpFormasPagamentos ) ;

     GravaArqINI ;

     exit ;
  end ;

  Ini := TIniFile.Create( fsArqINI ) ;
  try
     if Ini.ReadString('Variaveis','Aviso_Legal','SIM') = 'SIM' then
        AvisoLegal ;

     if fsNumECF = '' then
        fsNumECF := Ini.ReadString('Variaveis','NumECF',fsNumECF) ;

     fsIE        := Ini.ReadString('Variaveis','IE',fsIE) ;
     fsCNPJ      := Ini.ReadString('Variaveis','CNPJ',fsCNPJ) ;
     fsPAF       := Ini.ReadString('Variaveis','PAF',fsPAF) ;
     fsIM        := Ini.ReadString('Variaveis','IM',fsIM) ;
     fpEstado    := TACBrECFEstado( Ini.ReadInteger('Variaveis','Estado',
                       Integer( fpEstado) ) ) ;
     fsNumSerie  := Ini.ReadString('Variaveis','NumSerie',fsNumSerie) ;
     fsCRO       := Ini.ReadInteger('Variaveis','CRO',fsCRO) ;
     fsNumCupom  := Ini.ReadInteger('Variaveis','NumCupom',fsNumCupom) ;
     fsNumGNF    := Ini.ReadInteger('Variaveis','NumGNF',fsNumGNF) ;
     fsNumGRG    := Ini.ReadInteger('Variaveis','NumGRG',fsNumGRG) ;
     fsNumCDC    := Ini.ReadInteger('Variaveis','NumCDC',fsNumCDC) ;
     fsNumCER    := Ini.ReadInteger('Variaveis','NumCER',fsNumCER) ;
     fsGrandeTotal  := Ini.ReadFloat('Variaveis','GrandeTotal',fsGrandeTotal) ;
     fsVendaBruta   := Ini.ReadFloat('Variaveis','VendaBruta',fsVendaBruta) ;
     fsNumCCF    := Ini.ReadInteger('Variaveis','NumCCF',fsNumCCF) ;
     fsDia       := Ini.ReadDate('Variaveis','DiaMovimento',fsDia) ;
     fsVERAO     := Ini.ReadBool('Variaveis','HorarioVerao',fsVERAO) ;
     fsReducoesZ := Ini.ReadInteger('Variaveis','ReducoesZ',fsReducoesZ) ;
     fsLeiturasX := Ini.ReadInteger('Variaveis','LeiturasX',fsLeiturasX) ;
     fsCOOInicial:= Ini.ReadInteger('Variaveis','COOInicial',fsCOOInicial) ;
     fsCOOFinal  := Ini.ReadInteger('Variaveis','COOFinal',fsCOOFinal) ;
     fsSubTotal  := Ini.ReadFloat('Variaveis','SubTotal',fsSubTotal) ;
     fsTotalPago := Ini.ReadFloat('Variaveis','TotalPago',fsTotalPago) ;
     fsCuponsCancelados := Ini.ReadInteger('Variaveis','CuponsCancelados',
        fsCuponsCancelados) ;
     fsCuponsCanceladosTotal := Ini.ReadFloat('Variaveis', 'CuponsCanceladosTotal', fsCuponsCanceladosTotal);
     Operador    := Ini.ReadString('Variaveis','Operador',Operador) ;

     fsCabecalho.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'LIN'+IntToStrZero( A, 3) ;
        T := Ini.ReadString('Cabecalho', S, '*FIM*') ;

        if T = '*FIM*' then break ;

        fsCabecalho.Add( T ) ;
        A := A + 1 ;
     end ;

     fsCabecalhoItem.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'LIN'+IntToStrZero( A, 3) ;
        T := Ini.ReadString('Cabecalho_Item', S, '*FIM*') ;

        if T = '*FIM*' then break ;

        fsCabecalhoItem.Add( T ) ;
        A := A + 1 ;
     end ;

     fsMascaraItem := Ini.ReadString('Cabecalho_Item','MascaraItem',
        fsMascaraItem) ;

     fpColunas   := Ini.ReadInteger('Impressora','Colunas', fpColunas) ;

     if fsGavetaCmd = '' then
        fsGavetaCmd := AscToString( Ini.ReadString('Impressora',
           'Comando_Abrir_Gaveta', #027+'v'+#150) ) ;

     if fsCortaPapelCompletoCmd = '' then
        fsCortaPapelCompletoCmd := AscToString( Ini.ReadString('Impressora',
           'Comando_Corta_Papel_Completo', #027+#119) ) ;
     if fsCortaPapelParcialCmd = '' then
        fsCortaPapelParcialCmd := AscToString( Ini.ReadString('Impressora',
           'Comando_Corta_Papel_Parcial', #027+#109) ) ;

     fsCmdImpZera := AscToString( Ini.ReadString('Impressora',
        'Comando_Incializacao', fsCmdImpZera) ) ;
     fsCmdImpCondensado := AscToString( Ini.ReadString('Impressora',
        'Comando_Ativar_Condensado', fsCmdImpCondensado) ) ;
     fsCmdImpExpandidoUmaLinha := AscToString( Ini.ReadString('Impressora',
        'Comando_Expandido_uma_Linha', fsCmdImpExpandidoUmaLinha) );
     fsCmdImpFimExpandido := AscToString( Ini.ReadString('Impressora',
        'Comando_Fim_Expandido', fsCmdImpFimExpandido) );

     fsItensCupom.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'Item_Cupom'+IntToStrZero( A, 3) ;
        T := Ini.ReadString( S ,'Descricao','*FIM*') ;

        if T = '*FIM*' then break ;

        ItemCupom := TACBrECFNaoFiscalItemCupom.Create ;
        ItemCupom.Descricao := T ;
        ItemCupom.Codigo    := Ini.ReadString( S ,'Codigo','') ;
        ItemCupom.Qtd       := Ini.ReadFloat( S ,'Qtd',0) ;
        ItemCupom.ValorUnit := Ini.ReadFloat( S ,'ValorUnit',0) ;
        ItemCupom.PosAliq   := Ini.ReadInteger( S ,'PosAliq',0) ;
        ItemCupom.BaseICMS  := Ini.ReadFloat( S ,'BaseICMS',0) ;
        ItemCupom.Desconto  := Ini.ReadFloat( S ,'Desconto',0) ;

        fsItensCupom.Add( ItemCupom ) ;
        A := A + 1 ;
     end ;
     fsItensCount := fsItensCupom.Count ;

     fsPagamentosCupom.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'Pagamento_Cupom'+IntToStrZero( A, 2) ;
        B := Ini.ReadInteger( S ,'PosFPG',-1) ;

        if B = -1 then break ;

        PagamentoCupom := TACBrECFNaoFiscalPagamentoCupom.Create ;
        PagamentoCupom.PosFPG    := B ;
        PagamentoCupom.ValorPago := Ini.ReadFloat( S ,'ValorPago',0) ;

        fsPagamentosCupom.Add( PagamentoCupom ) ;
        A := A + 1 ;
     end ;

     fsCNFCupom.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'CNF_Cupom'+IntToStrZero( A, 2) ;
        B := Ini.ReadInteger( S ,'PosCNF',-1) ;

        if B = -1 then break ;

        CNFCupom := TACBrECFNaoFiscalCNFCupom.Create ;
        CNFCupom.PosCNF := B ;
        CNFCupom.Valor  := Ini.ReadFloat( S ,'Valor',0) ;

        fsCNFCupom.Add( CNFCupom ) ;
        A := A + 1 ;
     end ;

     fsAliquotas.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'Aliquota'+IntToStrZero( A, 2) ;
        T := Ini.ReadString( S ,'Descricao','*FIM*') ;

        if T = '*FIM*' then break ;

        AliqICMS := TACBrECFNaoFiscalAliquota.Create ;
        AliqICMS.Descricao := T ;
        AliqICMS.Aliquota  := Ini.ReadFloat( S ,'Aliquota',0) ;
        AliqICMS.Iss       := Ini.ReadBool( S ,'ISS',false) ;
        AliqICMS.TotalDia  := Ini.ReadFloat( S ,'TotalDia',0) ;

        fsAliquotas.Add( AliqICMS ) ;
        A := A + 1 ;
     end ;

     fsFormasPagamento.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'Forma_Pagamento'+IntToStrZero( A, 2) ;
        T := Ini.ReadString( S ,'Descricao','*FIM*') ;

        if T = '*FIM*' then break ;

        FormaPagamento := TACBrECFNaoFiscalFormaPagamento.Create ;
        FormaPagamento.Descricao := T ;
        FormaPagamento.Indice    := Ini.ReadString( S ,'Indice', '') ;
        FormaPagamento.TotalDia  := Ini.ReadFloat( S ,'TotalDia',0) ;

        fsFormasPagamento.Add( FormaPagamento ) ;
        A := A + 1 ;
     end ;

     fsComprovantesNaoFiscais.Clear ;
     A := 0 ;
     while true do
     begin
        S := 'Comprovante_nao_Fiscal'+IntToStrZero( A, 2) ;
        T := Ini.ReadString( S ,'Descricao','*FIM*') ;

        if T = '*FIM*' then break ;

        ComprovanteNaoFiscal := TACBrECFNaoFiscalComprovanteNaoFiscal.Create ;
        ComprovanteNaoFiscal.Descricao := T ;
        ComprovanteNaoFiscal.Indice    := Ini.ReadString( S ,'Indice', '') ;
        ComprovanteNaoFiscal.TotalDia  := Ini.ReadFloat( S ,'TotalDia',0) ;

        fsComprovantesNaoFiscais.Add( ComprovanteNaoFiscal ) ;
        A := A + 1 ;
     end ;

  finally
     Ini.Free ;
  end ;

end;

procedure TACBrECFNaoFiscal.SetNomeArqINI(const Value: String);
begin
  fsNomeArqINI := Value;
  fsGavetaCmd  := '' ;
  fsCortaPapelCompletoCmd := '';
  fsCortaPapelParcialCmd  := '';
  fsNumSerie   := '' ;
  fsNumECF     := '' ;
end;

function TACBrECFNaoFiscal.GetNomeArqINI: String;
Var Num : String ;
begin
  if fsNomeArqINI <> '' then
     Result := fsNomeArqINI
  else if fsArqINI <> '' then
     Result := fsArqINI
  else
   begin
     if fsNumECF = '' then
        Num := '001'
     else
        Num := fsNumECF ;

     Result := ExtractFilePath(fsEXEName)+'ACBrECF'+Num+'.INI';
   end ;
end;

function TACBrECFNaoFiscal.CalcTotalItem(AQtd, APrecoUnit: Double): Double;
begin
  if fpArredondaItemMFD then
    Result := RoundABNT( AQtd * APrecoUnit, -2)
  else
    Result := TruncTo( AQtd * APrecoUnit, -2);
end;

procedure TACBrECFNaoFiscal.AvisoLegal ;
begin
  {$IFNDEF NOGUI}
    if MessageDlg(ACBrStr( 'Este Emulador destina-se EXCLUSIVAMENTE para auxiliar no '+
                  'desenvolvimento de aplicativos para as impressoras fiscais. '+
                  sLineBreak + sLineBreak +
                  'Usar o emulador para fins comerciais sem a devida impressão '+
                  'do Cupom Fiscal ou Nota Fiscal pode caracterizar crime de '+
                  'Sonegação Fiscal.' + sLineBreak + sLineBreak +
                  'Continua com o uso do Emulador ?' )
                  ,{$IFDEF FMX}TMsgDlgType.{$ENDIF} mtWarning,mbYesNoCancel,0) <> mrYes then
       raise EACBrECFERRO.Create( ACBrStr('Uso indevido do emulador'));
  {$ENDIF}
end;

procedure TACBrECFNaoFiscal.AbreBuffer;
Var NomeArqBuffer : String ;
begin
  NomeArqBuffer := ChangeFileExt( fsArqINI, '.BUF') ;

  AssignFile( fsArqBuf, NomeArqBuffer );
  FileMode := fmOpenReadWrite	+ fmShareExclusive ;

  try
     if FileExists( NomeArqBuffer ) then
      begin
        fsBuffer.LoadFromFile( NomeArqBuffer );
        Reset( fsArqBuf );
        if not SeekEof( fsArqBuf ) then
           raise EACBrECFERRO.Create(ACBrStr('Erro ao posicionar em EOF no arquivo: ')+
                                  NomeArqBuffer) ;
      end
     else
        ReWrite( fsArqBuf );
  except
     try
        {$I-}
        CloseFile( fsArqBuf );
        IOResult;
        {$I+}
     except
     end ;
     raise ;
  end ;

end;

procedure TACBrECFNaoFiscal.GravaBuffer ;
var A : Integer ;
begin
  For A := 0 to fsBuffer.Count - 1 do
  begin
     Writeln( fsArqBuf, fsBuffer[A] ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.ImprimeBuffer;
Var A : Integer ;
    Buf : AnsiString ;
begin
  Buf := '' ;
  For A := 0 to fsBuffer.Count - 1 do
  begin
     Buf := fsBuffer[A] + CRLF ;
     ImprimePorta( Buf );
  end;

  ZeraBuffer ;
end;

procedure TACBrECFNaoFiscal.ZeraBuffer;
begin
  Rewrite( fsArqBuf ) ;
  fsBuffer.Clear ;
end;

procedure TACBrECFNaoFiscal.ImprimePorta( AString : AnsiString ) ;
Var OldAguardandoResposta : Boolean ;
begin
  OldAguardandoResposta := AguardandoResposta ;
  AguardandoResposta := true ;
  try
     fpDevice.EnviaString( AString );

     repeat
        Sleep(IntervaloAposComando);
     until fpDevice.EmLinha() ;
  finally
     AguardandoResposta := OldAguardandoResposta ;
  end ;
end;

procedure TACBrECFNaoFiscal.ImprimePorta(AStringList: TStringList);
Var Str : AnsiString ;
    A : Integer ;
begin
  AguardandoResposta := true ;

  try
     Str := '' ;
     For A := 0 to AStringList.Count - 1 do
     begin
        Str := AStringList[A] + CRLF ;
        ImprimePorta( Str );
     end;
  finally
     AguardandoResposta := False ;
  end ;
end;

procedure TACBrECFNaoFiscal.AddBufferRelatorio;
Var Total,Brutadia: Double;
    A : Integer ;
    T : String ;
begin
  Total := 0 ;
  BrutaDia:=0;
  For A := 0 to 2 do
     with TACBrECFNaoFiscalAliquota( fsAliquotas[A] ) do
        Total := RoundTo(Total + TotalDia,-2) ;

  with fsBuffer do
  begin
     Add( StringOfChar('-',Colunas) ) ;
     Add( PadCenter(' Contadores ',Colunas,'-') ) ;
     Add( PadSpace('Reducoes Z:|'+IntToStrZero(fsReducoesZ,4),Colunas,'|') ) ;
     Add( PadSpace('Leitura  X:|'+IntToStrZero(fsLeiturasX,6),Colunas,'|') ) ;
     Add( PadSpace('Cancelamentos de Cupom:|'+IntToStrZero(fsCuponsCancelados,6),
        Colunas,'|') ) ;
     Add( PadSpace('COO do Primeiro Cupom:|'+IntToStrZero(fsCOOInicial,6),
        Colunas,'|') ) ;
     Add( PadSpace('COO do Ultimo Cupom:|'+IntToStrZero(fsCOOFinal,6),Colunas,'|'));
     Add( PadCenter(' Totalizadores ',Colunas,'-') ) ;
     Add( PadSpace('Totalizador Geral:|'+FormatFloat('###,###,##0.00',
        fsGrandeTotal ),Colunas,'|') ) ;
      For A := 0 To fsAliquotas.Count - 1 Do
       BrutaDia := RoundTo(BrutaDia + TACBrECFNaoFiscalAliquota(fsAliquotas[A]).TotalDia, -2);
     Add( PadSpace('Venda Bruta Diaria:|'+FormatFloat('###,###,##0.00',
        BrutaDia), Colunas, '|'));

     Add( PadCenter('Total Vendido por Aliquota',Colunas,'-') ) ;
     Add( PadSpace('Substituicao Tributaria (FF)|'+FormatFloat('###,###,##0.00',
        TACBrECFNaoFiscalAliquota(fsAliquotas[0]).TotalDia ),Colunas,'|') ) ;
     Add( PadSpace('Isencao (II)|'+FormatFloat('###,###,##0.00',
        TACBrECFNaoFiscalAliquota(fsAliquotas[1]).TotalDia ),Colunas,'|') ) ;
     Add( PadSpace('Nao Incidencia (NN)|'+FormatFloat('###,###,##0.00',
        TACBrECFNaoFiscalAliquota(fsAliquotas[2]).TotalDia ),Colunas,'|') ) ;

     For A := 3 to fsAliquotas.Count - 1 do
     with TACBrECFNaoFiscalAliquota(fsAliquotas[A]) do
     begin
        if Iss then
           T := 'S'
        else
           T := 'T' ;

        Add( PadSpace(IntToStrZero(A,2)+'|'+ T + FormatFloat('#0.00',Aliquota)+'%|'+
                  FormatFloat('###,###,##0.00',TotalDia),Colunas,'|') ) ;
        Total := RoundTo(Total + TotalDia,-2) ;
     end ;

     Add( PadSpace('Total Cancelado R$|'+FormatFloat('###,###,##0.00', fsCuponsCanceladosTotal),
        Colunas,'|') ) ;

     Add( PadSpace('T O T A L   R$|'+FormatFloat('###,###,##0.00',Total),
        Colunas,'|') ) ;

     Add( PadCenter(' Relatorio Gerencial ',Colunas,'-') ) ;
     Add( PadSpace(' Relatorio Geral:|'+IntToStrZero(fsNumCER,6),Colunas,'|') ) ;

     Add( PadCenter('Formas de Pagamento',Colunas,'-') ) ;
     For A := 0 to fsFormasPagamento.Count - 1 do
     with TACBrECFNaoFiscalFormaPagamento(fsFormasPagamento[A]) do
     begin
        Add( PadSpace(Indice+'  '+PadRight(Descricao,20)+'|'+
                   FormatFloat('###,###,##0.00',TotalDia),Colunas,'|') ) ;
     end ;

     Add( PadCenter('Comprovantes nao Fiscal',Colunas,'-') ) ;
     For A := 0 to fsComprovantesNaoFiscais.Count - 1 do
     with TACBrECFNaoFiscalComprovanteNaoFiscal(fsComprovantesNaoFiscais[A]) do
     begin
        Add( PadSpace(Indice+'  '+PadRight(Descricao,20)+'|'+
                   FormatFloat('###,###,##0.00',TotalDia),Colunas,'|') ) ;
     end ;
  end ;

  AddBufferRodape ;
end;

procedure TACBrECFNaoFiscal.InsertCabecalho( AStringList : TStringList );
Var A : Integer ;
    V,Linha : AnsiString ;
begin
  if fsVERAO then
     V := 'V'
  else
     V := ' ' ;

  For A := 0 to fsCabecalho.Count - 1 do
  begin
     Linha := PadCenter(fsCabecalho[A], Colunas) ;
     if A = 0 then
        Linha := fsCmdImpCondensado + Linha ;
     AStringList.Insert( A, Linha ) ;
  end ;

  AStringList.Insert( fsCabecalho.Count,
                      PadSpace( DateToStr(now)+' '+TimeToStr(now)+V+'|COO:'+
                      IntToStrZero(fsNumCupom,6), Colunas, '|' ) ) ;
end;

procedure TACBrECFNaoFiscal.AddBufferCabecalho_Item;
Var A : Integer ;
begin
  fsBuffer.Add( PadCenter('COMPROVANTE  * NAO FISCAL *',Colunas) ) ;

  For A := 0 to fsCabecalhoItem.Count - 1 do
     fsBuffer.Add( fsCabecalhoItem[A] ) ;
end;

procedure TACBrECFNaoFiscal.AddBufferRodape;
Var V : AnsiString ;
    A : Integer ;
begin
  if fsVERAO then
     V := 'V'
  else
     V := ' ' ;

//....+....1....+....2....+....3....+....4....+...
//N.Serie XXXXXXXXXXXXXXX Maq 999 v0.8.3b
//Oper. XXXXXXXXXXXXXXX 99/99/99 99:99:99V
//** N A O   E   C U P O M   F I S C A L **
  with fsBuffer do
  begin
     Add( StringOfChar('-',Colunas) ) ;
     Add( PadSpace('N.Serie '+PadRight(fsNumSerie,21)+'|Maq '+PadRight(fsNumECF,3)+'|'+
               'v'+ACBR_VERSAO,Colunas,'|') );
     Add( PadSpace('Oper. '+PadRight(Operador,15)  +'|'+
               FormatDateTime('dd/mm/yy hh:nn:ss',now)+V,  Colunas,'|') );
     Add( PadCenter('** N A O   E   C U P O M   F I S C A L **',Colunas) );
     Add( StringOfChar('=',Colunas) ) ;
     For A := 1 to LinhasEntreCupons do
        Add( '' ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.RestauraEstadoAnterior;
begin
  fsVERAO      := fswVERAO ;
  fsDia        := fswDia   ;
  fpEstado     := fswEstado  ;
  fsReducoesZ  := fswReducoesZ ;
  fsLeiturasX  := fswLeiturasX ;
  fsCuponsCancelados := fswCuponsCancelados ;
  fsCuponsCanceladosTotal := fswCuponsCanceladosTotal;
  fsCOOInicial := fswCOOInicial ;
  fsCOOFinal   := fswCOOFinal ;
  fsNumCupom   := fswNumCupom ;
  fsNumGNF     := fswNumGNF ;
  fsNumGRG     := fswNumGRG ;
  fsNumCDC     := fswNumCDC ;
  fsNumCER     := fswNumCER ;
  fsGrandeTotal := fswGrandeTotal ;
  fsVendaBruta  := fswVendaBruta ;
  fsNumCCF     := fswNumCCF ;
  fsSubTotal   := fswSubTotal ;
  fsTotalPago  := fswTotalPago;

  CopyAliquotas( fswAliquotas, fsAliquotas ) ;
  CopyFormasPagamento( fswFormasPagamento, fsFormasPagamento ) ;
  CopyComprovantesNaoFiscais(fswComprovantesNaoFiscais, fsComprovantesNaoFiscais );
  CopyItensCupom( fswItensCupom, fsItensCupom ) ;
  CopyPagamentosCupom( fswPagamentosCupom, fsPagamentosCupom ) ;
end;

procedure TACBrECFNaoFiscal.SalvaEstadoAtual;
begin
  fswVERAO      := fsVERAO ;
  fswDia        := fsDia   ;
  fswESTADO     := fpEstado  ;
  fswReducoesZ  := fsReducoesZ ;
  fswLeiturasX  := fsLeiturasX ;
  fswCuponsCancelados := fsCuponsCancelados ;
  fswCuponsCanceladosTotal := fsCuponsCanceladosTotal;
  fswCOOInicial := fsCOOInicial ;
  fswCOOFinal   := fsCOOFinal ;
  fswNumCupom   := fsNumCupom ;
  fswNumGNF     := fsNumGNF ;
  fswNumGRG     := fsNumGRG ;
  fswNumCDC     := fsNumCDC ;
  fswNumCER     := fsNumCER ;
  fswGrandeTotal:= fsGrandeTotal ;
  fswVendaBruta := fsVendaBruta ;
  fswNumCCF     := fsNumCCF ;
  fswSubTotal   := fsSubTotal ;
  fswTotalPago  := fsTotalPago;

  CopyAliquotas( fsAliquotas, fswAliquotas ) ;
  CopyFormasPagamento( fsFormasPagamento, fswFormasPagamento ) ;
  CopyComprovantesNaoFiscais(fsComprovantesNaoFiscais, fswComprovantesNaoFiscais );
  CopyItensCupom( fsItensCupom, fswItensCupom ) ;
  CopyPagamentosCupom( fsPagamentosCupom, fswPagamentosCupom ) ;
end;

procedure TACBrECFNaoFiscal.AbreDocumento(AbreDia : Boolean) ;
var Cab : TStringList ;
begin

  if fsDia > now then
     raise EACBrECFERRO.create(ACBrStr('Erro ! A Data da Impressora: '+DateToStr(fsDia)+
            '‚ maior do que a Data atual: '+DateToStr(now))) ;

  if not EmLinha() then
     raise EACBrECFERRO.Create(ACBrStr('Impressora: '+fpModeloStr+' não está pronta.')) ;

  fsNumCupom := fsNumCupom + 1 ;
  fsCOOFinal := fsNumCupom ;

  if AbreDia or (CompareDate(fsDIA, now) > 0) then
  begin
     fsDIA        := now ;
     fsCOOInicial := fsNumCupom ;
  end ;

  Cab := TStringList.create ;
  try
     InsertCabecalho( Cab );

     ImprimePorta( Cab );
     GravaBuffer ;
     try
        GravaArqINI ;
        ImprimeBuffer;
     finally
        ZeraBuffer ;
     end ;
  finally
     Cab.Free ;
  end ;
end;

function TACBrECFNaoFiscal.GetEstado: TACBrECFEstado;
Var estAnterior : TACBrECFEstado ;
begin
  estAnterior := fpEstado ;
  if not (fpEstado in [estNaoInicializada,estDesconhecido]) then
  begin
     if (CompareDate( now, fsDia) > 0) and
        ( not (fpEstado in [estBloqueada,estRequerX])) then
        fpEstado := estRequerZ ;

     if (fpEstado = estBloqueada) and (CompareDate( now, fsDia) > 0) then
        fpEstado := estRequerX ;
  end ;

  if fpEstado in [estDesconhecido, estNaoInicializada] then
     fpEstado := estLivre ;

  if fpEstado <> estAnterior then
     GravaArqINI ;

  result := fpEstado ;
end ;

function TACBrECFNaoFiscal.GetArredonda: Boolean;
begin
  Result := fpArredondaItemMFD;
end;

function TACBrECFNaoFiscal.GetHorarioVerao: Boolean;
begin
  Result := fsVERAO ;
end;

procedure TACBrECFNaoFiscal.CarregaFormasPagamento;
Var A : Integer ;
    FormaPagamento : TACBrECFFormaPagamento ;
begin
  inherited CarregaFormasPagamento ;   { Cria fpFormasPagamentos }

  for A := 0 to fsFormasPagamento.Count - 1 do
  begin
     FormaPagamento := TACBrECFFormaPagamento.create ;

     with TACBrECFNaoFiscalFormaPagamento( fsFormasPagamento[A] ) do
     begin
        FormaPagamento.Indice    := Indice ;
        FormaPagamento.Descricao := Descricao ;
        FormaPagamento.Total     := TotalDia ;
        FormaPagamento.PermiteVinculado := true ;
     end ;

     fpFormasPagamentos.Add( FormaPagamento ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.LerTotaisFormaPagamento;
begin
  CarregaFormasPagamento ;
end;

function TACBrECFNaoFiscal.AchaFPGDescricao(Descricao: String;
  BuscaExata: Boolean; IgnorarCase: Boolean; IgnorarAcentos: Boolean
  ): TACBrECFFormaPagamento;
begin
  result := inherited AchaFPGDescricao(Descricao, BuscaExata, IgnorarCase, IgnorarAcentos) ;

  { Na impressora Nao Fiscal podemos programas as Formas de Pagamento
    dinamicamente. }
  if (Result = nil) then
  begin
     ProgramaFormaPagamento( Descricao ) ;
     result := inherited AchaFPGDescricao(Descricao, BuscaExata, IgnorarCase, IgnorarAcentos) ;
  end ;
end;

procedure TACBrECFNaoFiscal.ProgramaFormaPagamento( var Descricao: String;
   PermiteVinculado : Boolean; Posicao : String ) ;
Var FPagto : TACBrECFFormaPagamento ;
    FPagtoNaoFiscal : TACBrECFNaoFiscalFormaPagamento ;
    A : Integer ;
begin
  { Verificando se a Descriçao já foi programada antes (ja existe ?) }
  For A := 0 to fsFormasPagamento.Count -1 do
     if trim(UpperCase(
          TACBrECFNaoFiscalFormaPagamento(fsFormasPagamento[A]).Descricao)) =
        trim(UpperCase(Descricao)) then
        exit ;

  Descricao := PadRight(Descricao,20) ;         { Ajustando tamanho final }

  SalvaEstadoAtual ;

  FPagtoNaoFiscal := TACBrECFNaoFiscalFormaPagamento.create ;
  FPagtoNaoFiscal.Indice    := IntToStrZero( fsFormasPagamento.Count+1,2) ;
  FPagtoNaoFiscal.Descricao := Descricao ;
  FPagtoNaoFiscal.TotalDia  := 0 ;
  fsFormasPagamento.Add( FPagtoNaoFiscal ) ;

  try
     GravaArqINI ;

     if Assigned(fpFormasPagamentos) then
     begin
        FPagto := TACBrECFFormaPagamento.create ;
        FPagto.Indice           := FPagtoNaoFiscal.Indice ;
        FPagto.Descricao        := Descricao ;
        FPagto.PermiteVinculado := PermiteVinculado ;
        fpFormasPagamentos.Add( FPagto ) ;
     end ;
  except
     RestauraEstadoAnterior ;
  end ;
end;


procedure TACBrECFNaoFiscal.CarregaAliquotas;
Var A : Integer ;
    Aliquota : TACBrECFAliquota ;
begin
  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  for A := 0 to fsAliquotas.Count - 1 do
  begin
     Aliquota := TACBrECFAliquota.create ;

     Aliquota.Indice   := IntToStrZero(A+1,2) ;
     Aliquota.Total    := TACBrECFNaoFiscalAliquota( fsAliquotas[A] ).TotalDia ;
     Aliquota.Aliquota := TACBrECFNaoFiscalAliquota( fsAliquotas[A] ).Aliquota ;
     if TACBrECFNaoFiscalAliquota( fsAliquotas[A] ).Iss then
        Aliquota.Tipo := 'S' ;

     fpAliquotas.Add( Aliquota ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.LerTotaisAliquota;
begin
  CarregaAliquotas ;
end;

function TACBrECFNaoFiscal.AchaICMSAliquota( Aliquota: Double; Tipo : Char ):
   TACBrECFAliquota;
begin
  result := inherited AchaICMSAliquota( Aliquota, Tipo ) ;

(*  { Na impressora Nao Fiscal podemos programas as Aliquotas dinamicamente. }
  if (Result = nil) then
  begin
     ProgramaAliquota( Aliquota, Tipo ) ;
     result := inherited AchaICMSAliquota( Aliquota, Tipo ) ;
  end ; *)
end;

function TACBrECFNaoFiscal.AchaICMSAliquota(var AliquotaICMS: String):
   TACBrECFAliquota;
  Var AliquotaStr : String ;
begin
  Result      := nil ;
  AliquotaStr := ''  ;

  case AliquotaICMS[1] of
     'F' : AliquotaStr := '01' ;
     'N' : AliquotaStr := '02' ;
     'I' : AliquotaStr := '03' ;
     'T' : AliquotaICMS := 'T'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}
  end;

  if AliquotaStr = '' then
     Result := inherited AchaICMSAliquota( AliquotaICMS )
  else
     AliquotaICMS := AliquotaStr ;
end;

procedure TACBrECFNaoFiscal.ProgramaAliquota( Aliquota : Double; Tipo : Char;
   Posicao : String) ;
Var Aliq : TACBrECFAliquota ;
    AliqNaoFiscal : TACBrECFNaoFiscalAliquota ;
    A : Integer ;
begin
  { Verificando se a Aliquota já foi programada antes (ja existe ?) }
  For A := 0 to fsAliquotas.Count -1 do
     if (TACBrECFNaoFiscalAliquota(fsAliquotas[A]).Aliquota = Aliquota) and
        (TACBrECFNaoFiscalAliquota(fsAliquotas[A]).Iss      = (Tipo = 'S')) then
        exit ;

  Tipo := UpCase(Tipo) ;

  SalvaEstadoAtual ;

  AliqNaoFiscal := TACBrECFNaoFiscalAliquota.create ;
  AliqNaoFiscal.Aliquota  := Aliquota ;
  AliqNaoFiscal.Descricao := FloatToStr(Aliquota) ;
  AliqNaoFiscal.Iss       := (Tipo = 'S') ;
  fsAliquotas.Add( AliqNaoFiscal ) ;

  try
     GravaArqINI ;

     if Assigned( fpAliquotas ) then
     begin
        Aliq := TACBrECFAliquota.create ;
        Aliq.Indice   := IntToStrZero( fpAliquotas.Count+1,2) ;
        Aliq.Aliquota := Aliquota ;
        if Tipo = 'S' then
           Aliq.Tipo := Tipo ;
        fpAliquotas.Add( Aliq ) ;
     end ;
  except
     RestauraEstadoAnterior ;
  end ;
end;

procedure TACBrECFNaoFiscal.CarregaComprovantesNaoFiscais;
Var A : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  inherited CarregaComprovantesNaoFiscais ;   { Cria fpComprovantesNaoFiscais }

  for A := 0 to fsComprovantesNaoFiscais.Count - 1 do
  begin
     CNF := TACBrECFComprovanteNaoFiscal.create ;

     with TACBrECFNaoFiscalComprovanteNaoFiscal( fsComprovantesNaoFiscais[A] ) do
     begin
        CNF.Indice    := Indice ;
        CNF.Descricao := Descricao ;
        CNF.Total     := TotalDia ;
        CNF.PermiteVinculado := true ;
     end ;

     fpComprovantesNaoFiscais.Add( CNF ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.LerTotaisComprovanteNaoFiscal;
begin
   CarregaComprovantesNaoFiscais ;
end;

function TACBrECFNaoFiscal.AchaCNFDescricao(Descricao: String;
   BuscaExata : Boolean; IgnorarCase : Boolean ) : TACBrECFComprovanteNaoFiscal;
begin
  result := inherited AchaCNFDescricao( Descricao, BuscaExata, IgnorarCase ) ;

  { Na impressora Nao Fiscal podemos programas os CNFs dinamicamente. }
  if (Result = nil) then
  begin
     ProgramaComprovanteNaoFiscal( Descricao, 'V' ) ;
     result := inherited AchaCNFDescricao( Descricao, BuscaExata, IgnorarCase ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.ProgramaComprovanteNaoFiscal(var Descricao: String;
   Tipo: String; Posicao : String);
Var CNF : TACBrECFComprovanteNaoFiscal ;
    CNFNaoFiscal : TACBrECFNaoFiscalComprovanteNaoFiscal ;
    A : Integer ;
begin
  { Verificando se a Descriçao já foi programada antes (ja existe ?) }
  For A := 0 to fsComprovantesNaoFiscais.Count -1 do
     if trim(UpperCase(TACBrECFNaoFiscalComprovanteNaoFiscal(
               fsComprovantesNaoFiscais[A]).Descricao)) =
        trim(UpperCase(Descricao)) then
        exit ;

  Descricao := PadRight(Descricao,20) ;         { Ajustando tamanho final }
  Tipo      := UpperCase( Tipo ) ;
  if Tipo = '' then
     Tipo := 'V' ;

  SalvaEstadoAtual ;

  CNFNaoFiscal := TACBrECFNaoFiscalComprovanteNaoFiscal.create ;
  CNFNaoFiscal.Indice    := IntToStrZero( fsComprovantesNaoFiscais.Count+1,2) ;
  CNFNaoFiscal.Descricao := Descricao ;
  CNFNaoFiscal.TotalDia  := 0 ;
  fsComprovantesNaoFiscais.Add( CNFNaoFiscal ) ;

  try
     GravaArqINI ;

     if Assigned( fpComprovantesNaoFiscais ) then
     begin
        CNF := TACBrECFComprovanteNaoFiscal.create ;
        CNF.Indice    := CNFNaoFiscal.Indice ;
        CNF.Descricao := Descricao ;
        CNF.PermiteVinculado := (Tipo =  'V') ;
        fpComprovantesNaoFiscais.Add( CNF ) ;
     end ;
  except
     RestauraEstadoAnterior ;
  end ;
end;

function TACBrECFNaoFiscal.AchaFPGIndiceNaoFiscal( const Indice: String) : Integer ;
var A : Integer ;
begin
  result := -1 ;
  with fsFormasPagamento do
  begin
     For A := 0 to Count -1 do
        if TACBrECFNaoFiscalFormaPagamento(Items[A]).Indice = Indice then
        begin
           Result := A ;
           Break ;
        end ;
  end ;
end;

function TACBrECFNaoFiscal.AchaCNFIndiceNaoFiscal(const Indice: String): Integer;
var A : Integer ;
begin
  result := -1 ;
  with fsComprovantesNaoFiscais do
  begin
     For A := 0 to Count -1 do
        if TACBrECFNaoFiscalComprovanteNaoFiscal(Items[A]).Indice = Indice then
        begin
           Result := A ;
           Break ;
        end ;
  end ;
end;

function TACBrECFNaoFiscal.NumeroColunas: Integer;
begin
  if ( '' = fsCmdImpExpandidoUmaLinha ) then
    Result := Colunas
  else
    Result := Round( Colunas / 2 );
end;

procedure TACBrECFNaoFiscal.CopyAliquotas(FromObjectList,
   ToObjectList: TObjectList);
Var A : Integer ;
    ItemF,ItemT : TACBrECFNaoFiscalAliquota ;
begin
  ToObjectList.Clear ;

  For A := 0 to FromObjectList.Count - 1 do
  begin
     ItemF := TACBrECFNaoFiscalAliquota( FromObjectList[A] ) ; 
     ItemT := TACBrECFNaoFiscalAliquota.Create ;

     ItemT.Aliquota  := ItemF.Aliquota ;
     ItemT.Descricao := ItemF.Descricao ;
     ItemT.Iss       := ItemF.Iss ;
     ItemT.TotalDia  := ItemF.TotalDia ;

     ToObjectList.Add( ItemT ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.CopyFormasPagamento(FromObjectList,
  ToObjectList: TObjectList);
Var A : Integer ;
    ItemF,ItemT : TACBrECFNaoFiscalFormaPagamento ;
begin
  ToObjectList.Clear ;

  For A := 0 to FromObjectList.Count - 1 do
  begin
     ItemF := TACBrECFNaoFiscalFormaPagamento( FromObjectList[A] ) ;
     ItemT := TACBrECFNaoFiscalFormaPagamento.Create ;

     ItemT.Indice    := ItemF.Indice ;
     ItemT.Descricao := ItemF.Descricao ;
     ItemT.TotalDia  := ItemF.TotalDia ;

     ToObjectList.Add( ItemT ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.CopyComprovantesNaoFiscais(FromObjectList,
  ToObjectList: TObjectList);
Var A : Integer ;
    ItemF,ItemT : TACBrECFNaoFiscalComprovanteNaoFiscal ;
begin
  ToObjectList.Clear ;

  For A := 0 to FromObjectList.Count - 1 do
  begin
     ItemF := TACBrECFNaoFiscalComprovanteNaoFiscal( FromObjectList[A] ) ;
     ItemT := TACBrECFNaoFiscalComprovanteNaoFiscal.Create ;

     ItemT.Indice    := ItemF.Indice ;
     ItemT.Descricao := ItemF.Descricao ;
     ItemT.TotalDia  := ItemF.TotalDia ;

     ToObjectList.Add( ItemT ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.CopyItensCupom(FromObjectList,
  ToObjectList: TObjectList);
Var A : Integer ;
    ItemF,ItemT : TACBrECFNaoFiscalItemCupom ;
begin
  ToObjectList.Clear ;

  For A := 0 to FromObjectList.Count - 1 do
  begin
     ItemF := TACBrECFNaoFiscalItemCupom( FromObjectList[A] ) ;
     ItemT := TACBrECFNaoFiscalItemCupom.Create ;

     ItemT.Codigo    := ItemF.Codigo ;
     ItemT.Descricao := ItemF.Descricao ;
     ItemT.Qtd       := ItemF.Qtd ;
     ItemT.ValorUnit := ItemF.ValorUnit ;
     ItemT.PosAliq   := ItemF.PosAliq ;
     ItemT.BaseICMS  := ItemF.BaseICMS ;
     ItemT.Desconto  := ItemF.Desconto ;

     ToObjectList.Add( ItemT ) ;
  end ;
end;

procedure TACBrECFNaoFiscal.CopyPagamentosCupom(FromObjectList,
  ToObjectList: TObjectList);
Var A : Integer ;
    ItemF,ItemT : TACBrECFNaoFiscalPagamentoCupom ;
begin
  ToObjectList.Clear ;

  For A := 0 to FromObjectList.Count - 1 do
  begin
     ItemF := TACBrECFNaoFiscalPagamentoCupom( FromObjectList[A] ) ;
     ItemT := TACBrECFNaoFiscalPagamentoCupom.Create ;

     ItemT.PosFPG    := ItemF.PosFPG ;
     ItemT.ValorPago := ItemF.ValorPago ;

     ToObjectList.Add( ItemT ) ;
  end ;
end;

function TACBrECFNaoFiscal.GetDataMovimento: TDateTime;  // Por Waldir Paim
begin
  Result := fsDia ;
end;

end.

