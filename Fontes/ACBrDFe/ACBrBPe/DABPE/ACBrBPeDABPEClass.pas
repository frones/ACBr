{******************************************************************************}
{ Projeto: Componente ACBrBPe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Bilhete de }
{ Passagem Eletrônica - BPe                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2017                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrBPeDABPEClass;

interface

uses
  SysUtils, Classes, ACBrBase,
  pcnBPe, pcnConversao;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TCasasDecimais = class(TComponent)
  private
    fFormato : TDetFormato;
    FqCom: Integer;
    FvUnCom: Integer;
    FMask_qCom: String;
    FMask_vUnCom: String;

    procedure Set_qCom(AValue: Integer);
    procedure Set_vUnCom(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Formato : TDetFormato  read fFormato     write fFormato;
    property _qCom: Integer         read FQCom        write Set_qCom;
    property _vUnCom: Integer       read FvUnCom      write Set_vUnCom;
    property _Mask_qCom: String     read FMask_qCom   write FMask_qCom;
    property _Mask_vUnCom: String   read FMask_vUnCom write FMask_vUnCom;
  end;

  { TACBrBPeDABPEClass }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrBPeDABPEClass = class( TACBrComponent )
   private
    procedure SetBPe(const Value: TComponent);
    procedure ErroAbstract(NomeProcedure: String);
    procedure SetPathPDF(const Value: String);
    function GetPathPDF: String;
  protected
    FACBrBPe: TComponent;
    FLogo: String;
    FSistema: String;
    FUsuario: String;
    FPathPDF: String;
    FImpressora: String;
    FImprimeNomeFantasia: Boolean;
    FImprimirTotalLiquido: Boolean;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FTipoDABPE: TpcnTipoImpressao;
    FNumCopias: Integer;
    FExpandirLogoMarca: Boolean;
    FFax: String;
    FSite: String;
    FEmail: String;
    FImprimeDescPorc: Boolean;
    FProtocoloBPe: String;
    FMargemIBPerior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCasasDecimais: TCasasDecimais;
    FExibeResumoCanhoto: Boolean;
    FExibeResumoCanhoto_Texto: String;
    FFormularioContinuo: Boolean;
    FTamanhoFonte_DemaisCampos: Integer;
    FProdutosPorPagina: Integer;
    FImprimirDetalhamentoEspecifico: Boolean;
    FBPeCancelada: Boolean;
    FImprimeItens: Boolean;
    FViaConsumidor : Boolean;
    FvTroco: Currency;
    FImprimeEmUmaLinha: Boolean;
    FUsaCodigoEanImpressao: Boolean;
    FImprimeDescAcrescItem: Boolean;

    FTamanhoLogoHeight: Integer;
    FTamanhoLogoWidth: Integer;
    FRecuoEndereco: Integer;
    FRecuoEmpresa: Integer;
    FLogoEmCima: Boolean;
    FTamanhoFonteEndereco: Integer;
    FRecuoLogo: Integer;

    FTributosSeparadamente : Boolean; //informação dos tributos separadamente
    FvTribFed: Currency; //total tributos federais
    FvTribEst: Currency; //total tributos estaduais
    FvTribMun: Currency; //total tributos municipais
    FFonteTributos: String;
    FChaveTributos: String;

    FPosCanhoto: TPosRecibo;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDABPE(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPECancelado(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPEResumido(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPEPDF(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPEResumidoPDF(BPe: TBPe = nil); virtual;
    procedure ImprimirEVENTO(BPe: TBPe = nil); virtual;
    procedure ImprimirEVENTOPDF(BPe: TBPe = nil); virtual;

    function ManterCodigo(scEAN, scProd: String): String;
    function ManterNomeImpresso(sXNome, sXFant: String): String;
    function FormatQuantidade(dValor: Double; dForcarDecimais: Boolean = True): String;
    function FormatValorUnitario(dValor: Double): String;
    function ManterUnidades(sUCom, sUTrib: String): String;
    function ManterQuantidades(dQCom, dQTrib: Double): String;
    function ManterValoresUnitarios(dVCom, dVTrib: Double): String;

  published
    property ACBrBPe: TComponent                     read FACBrBPe                        write SetBPe;
    property Logo: String                            read FLogo                           write FLogo;
    property Sistema: String                         read FSistema                        write FSistema;
    property Usuario: String                         read FUsuario                        write FUsuario;
    property PathPDF: String                         read GetPathPDF                      write SetPathPDF;
    property Impressora: String                      read FImpressora                     write FImpressora;
    property MostrarPreview: Boolean                 read FMostrarPreview                 write FMostrarPreview;
    property MostrarStatus: Boolean                  read FMostrarStatus                  write FMostrarStatus;
    property TipoDABPE: TpcnTipoImpressao            read FTipoDABPE                      write FTipoDABPE;
    property NumCopias: Integer                      read FNumCopias                      write FNumCopias;
    property Fax: String                             read FFax                            write FFax;
    property Site: String                            read FSite                           write FSite;
    property Email: String                           read FEmail                          write FEmail;
    property ImprimeNomeFantasia: Boolean            read FImprimeNomeFantasia            write FImprimeNomeFantasia;
    property ImprimirDescPorc: Boolean               read FImprimeDescPorc                write FImprimeDescPorc;
    property ImprimirTotalLiquido: Boolean           read FImprimirTotalLiquido           write FImprimirTotalLiquido;
    property ProtocoloBPe: String                    read FProtocoloBPe                   write FProtocoloBPe;
    property MargemIBPerior: Double                  read FMargemIBPerior                 write FMargemIBPerior;
    property MargemSuperior: Double                  read FMargemSuperior                 write FMargemSuperior;
    property MargemEsquerda: Double                  read FMargemEsquerda                 write FMargemEsquerda;
    property MargemDireita: Double                   read FMargemDireita                  write FMargemDireita;
    property CasasDecimais: TCasasDecimais           read FCasasDecimais;
    property ExibirResumoCanhoto: Boolean            read FExibeResumoCanhoto             write FExibeResumoCanhoto;
    property ExibirResumoCanhoto_Texto: String       read FExibeResumoCanhoto_Texto       write FExibeResumoCanhoto_Texto;
    property FormularioContinuo: Boolean             read FFormularioContinuo             write FFormularioContinuo;
    property ExpandirLogoMarca: Boolean              read FExpandirLogoMarca              write FExpandirLogoMarca default false;
    property TamanhoFonte_DemaisCampos: Integer      read FTamanhoFonte_DemaisCampos      write FTamanhoFonte_DemaisCampos;
    property ProdutosPorPagina: Integer              read FProdutosPorPagina              write FProdutosPorPagina;
    property ImprimirDetalhamentoEspecifico: Boolean read FImprimirDetalhamentoEspecifico write FImprimirDetalhamentoEspecifico;
    property BPeCancelada: Boolean                   read FBPeCancelada                   write FBPeCancelada;
    property ImprimirItens: Boolean                  read FImprimeItens                   write FImprimeItens;
    property vTroco: Currency                        read FvTroco                         write FvTroco;
    property ViaConsumidor : Boolean                 read FViaConsumidor                  write FViaConsumidor;
    property TamanhoLogoHeight: Integer              read FTamanhoLogoHeight              write FTamanhoLogoHeight;
    property TamanhoLogoWidth: Integer               read FTamanhoLogoWidth               write FTamanhoLogoWidth;
    property RecuoEndereco: Integer                  read FRecuoEndereco                  write FRecuoEndereco;
    property RecuoEmpresa: Integer                   read FRecuoEmpresa                   write FRecuoEmpresa;
    property LogoemCima: Boolean                     read FLogoEmCima                     write FLogoEmCima;
    property TamanhoFonteEndereco: Integer           read FTamanhoFonteEndereco           write FTamanhoFonteEndereco;
    property RecuoLogo: Integer                      read FRecuoLogo                      write FRecuoLogo;
    property TributosSeparadamente: Boolean          read FTributosSeparadamente          write FTributosSeparadamente;
    property vTribFed: Currency                      read FvTribFed                       write FvTribFed;
    property vTribEst: Currency                      read FvTribEst                       write FvTribEst;
    property vTribMun: Currency                      read FvTribMun                       write FvTribMun;
    property FonteTributos: String                   read FFonteTributos                  write FFonteTributos;
    property ChaveTributos: String                   read FChaveTributos                  write FChaveTributos;
    property PosCanhoto: TPosRecibo                  read FPosCanhoto                     write FPosCanhoto default prCabecalho;
    property ImprimeEmUmaLinha: Boolean              read FImprimeEmUmaLinha              write FImprimeEmUmaLinha default True;
    property ImprimeDescAcrescItem: Boolean          read FImprimeDescAcrescItem          write FImprimeDescAcrescItem default True;
    property UsaCodigoEanImpressao: Boolean          read FUsaCodigoEanImpressao          write FUsaCodigoEanImpressao default False;
  end;

implementation

uses
  ACBrBPe, ACBrUtil;

//Casas Decimais
constructor TCasasDecimais.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FMask_qCom    := ',0.00';
  FMask_vUnCom  := ',0.00';
  FQCom         := 2;
  FvUnCom       := 2;
end;

destructor TCasasDecimais.Destroy;
begin

  inherited Destroy;
end;

procedure TCasasDecimais.Set_qCom(AValue: Integer);
begin
  if ((AValue >= 0) and (AValue <= 4))  then
    FqCom := AValue
  else
    FqCom := 2;
end;

procedure TCasasDecimais.Set_vUnCom(AValue: Integer);
begin
  if ((AValue >= 0) and (AValue <= 10))  then
    FvUnCom := AValue
  else
    FvUnCom := 2;
end;

//DABPE CLASS
constructor TACBrBPeDABPEClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrBPe    := nil;
  FLogo       := '';
  FSistema    := '';
  FUsuario    := '';
  FPathPDF    := '';
  FImpressora := '';

  FImprimirTotalLiquido := False;
  FMostrarPreview       := True;
  FMostrarStatus        := True;
  FNumCopias := 1;
  FFax       := '';
  FSite      := '';
  FEmail     := '';
  FImprimeDescPorc := False;
  FProtocoloBPe    := '';
  FMargemIBPerior  := 0.8;
  FMargemSuperior  := 0.8;
  FMargemEsquerda  := 0.6;
  FMargemDireita   := 0.51;
  FExibeResumoCanhoto := false;
  FExibeResumoCanhoto_Texto := '';
  FFormularioContinuo := false;
  FTamanhoFonte_DemaisCampos := 8;
  FProdutosPorPagina := 0;
  FImprimeNomeFantasia            := False;
  FImprimirDetalhamentoEspecifico := true;
  FImprimirTotalLiquido:= True;
  FBPeCancelada := False;
  FCasasDecimais := TCasasDecimais.Create(self);
  FCasasDecimais.Name:= 'CasasDecimais';
  FPosCanhoto := prCabecalho;
  FImprimeItens := True;
  FViaConsumidor:= True;
  FvTroco       := 0.0;

  FTributosSeparadamente:= False;
  FvTribFed:= 0.0;
  FvTribEst:= 0.0;
  FvTribMun:= 0.0;
  FChaveTributos := '';

  FImprimeEmUmaLinha     := True;
  FImprimeDescAcrescItem := True;
  FUsaCodigoEanImpressao := False;

  {$IFDEF COMPILER6_UP}
      FCasasDecimais.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
end;

destructor TACBrBPeDABPEClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrBPeDABPEClass.ImprimirDABPE(BPe : TBPe = nil);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPECancelado(BPe: TBPe);
begin
  ErroAbstract('ImprimirCancelado');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPEResumido(BPe : TBPe = nil);
begin
  ErroAbstract('ImprimirResumido');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPEPDF(BPe : TBPe = nil);
begin
  ErroAbstract('ImprimirDABPEPDF');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPEResumidoPDF(BPe: TBPe);
begin
  ErroAbstract('ImprimirDABPEResumidoPDF');
end;

procedure TACBrBPeDABPEClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrBPe <> nil) and (AComponent is TACBrBPe) then
    FACBrBPe := nil;
end;

procedure TACBrBPeDABPEClass.SetBPe(const Value: TComponent);
  Var OldValue : TACBrBPe;
begin
  if Value <> FACBrBPe then
  begin
    if Value <> nil then
      if not (Value is TACBrBPe) then
        raise EACBrBPeException.Create('ACBrDABPE.BPe deve ser do tipo TACBrBPe');

    if Assigned(FACBrBPe) then
      FACBrBPe.RemoveFreeNotification(Self);

    OldValue := TACBrBPe(FACBrBPe);   // Usa outra variavel para evitar Loop Infinito
    FACBrBPe := Value;                 // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.DABPE) then
        OldValue.DABPE := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      TACBrBPe(Value).DABPE := self;
    end;
  end;
end;

procedure TACBrBPeDABPEClass.SetPathPDF(const Value: String);
begin
  FPathPDF := PathWithDelim(Value);
end;

procedure TACBrBPeDABPEClass.ErroAbstract(NomeProcedure: String);
begin
  raise EACBrBPeException.Create(NomeProcedure + ' não implementado em: ' + ClassName);
end;

function TACBrBPeDABPEClass.GetPathPDF: String;
var
  dhEmissao: TDateTime;
  DescricaoModelo: String;
  ABPe: TBPe;
begin
  if (csDesigning in ComponentState) then
  begin
    Result := FPathPDF;
    Exit;
  end;

  Result := Trim(FPathPDF);

  if EstaVazio(Result) then  // Se não informou o Diretório para o PDF
  begin
    if Assigned(ACBrBPe) then  // Se tem o componente ACBrBPe
    begin
      if TACBrBPe(ACBrBPe).Bilhetes.Count > 0 then  // Se tem algum Bilhete carregado
      begin
        ABPe := TACBrBPe(ACBrBPe).Bilhetes.Items[0].BPe;
        if TACBrBPe(ACBrBPe).Configuracoes.Arquivos.EmissaoPathBPe then
          dhEmissao := ABPe.Ide.dhEmi
        else
          dhEmissao := Now;

        DescricaoModelo := 'BPe';

        Result := TACBrBPe(FACBrBPe).Configuracoes.Arquivos.GetPath(
                         Result,
                         DescricaoModelo,
                         ABPe.Emit.CNPJ,
                         dhEmissao,
                         DescricaoModelo);
      end;
    end;
  end;

  if EstaVazio(Result) then  // Se não pode definir o Parth, use o Path da Aplicaçao
    Result := ExtractFilePath(ParamStr(0));

  Result := PathWithDelim( Result );
  (*

  Result := PathWithDelim(FPathPDF);

  // Criar diretório conforme configurado para BP-e
  if Assigned(ACBrBPe) then
  begin
    if TACBrBPe(ACBrBPe).Bilhetes.Count > 0 then
    begin
      ABPe := TACBrBPe(ACBrBPe).Bilhetes.Items[0].BPe;
      if TACBrBPe(ACBrBPe).Configuracoes.Arquivos.EmissaoPathBPe then
        dhEmissao := ABPe.Ide.dhEmi
      else
        dhEmissao := Now;

      DescricaoModelo := 'BPe';

      Result := PathWithDelim(TACBrBPe(FACBrBPe).Configuracoes.Arquivos.GetPath(
                              Result
                             ,DescricaoModelo
                             ,ABPe.Emit.CNPJ
                             ,dhEmissao
                             ,DescricaoModelo
                             ));
    end;
  end;
  *)
end;

procedure TACBrBPeDABPEClass.ImprimirEVENTO(BPe: TBPe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrBPeDABPEClass.ImprimirEVENTOPDF(BPe: TBPe);
begin
  ErroAbstract('ImprimirPDF');
end;

function TACBrBPeDABPEClass.FormatQuantidade(dValor: Double; dForcarDecimais: Boolean) : String;
begin
  if (Frac(dValor) > 0) or (dForcarDecimais) then
  begin
    case CasasDecimais.Formato of
      tdetInteger : Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._qCom));
      tdetMascara : Result := FormatFloatBr( dValor , CasasDecimais._Mask_qCom);
    else
      Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._qCom));
    end
  end
  else
    // caso contrário mostrar somente o número inteiro
    Result := FloatToStr( dValor );
end;


function TACBrBPeDABPEClass.FormatValorUnitario( dValor : Double ) : String;
begin
  // formatar conforme configurado
  case CasasDecimais.Formato of
    tdetInteger : Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._vUnCom));
    tdetMascara : Result := FormatFloatBr( dValor , CasasDecimais._Mask_vUnCom);
  else
    Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._vUnCom));
  end;
end;

function TACBrBPeDABPEClass.ManterCodigo( scEAN , scProd : String ) : String;
begin
  if (Length( scEAN ) > 0) and (UsaCodigoEanImpressao) then
    Result := Trim(scEAN)
  else
    Result := Trim(scProd);
end;

function TACBrBPeDABPEClass.ManterNomeImpresso( sXNome , sXFant : String ) : String;
begin
  if ( fImprimeNomeFantasia ) and ( sXFant <> '' ) then
    Result := sXFant
  else
    Result := sXNome;
end;

function TACBrBPeDABPEClass.ManterQuantidades(dQCom, dQTrib: Double): String;
begin
   Result := FormatQuantidade( dQCom );
   if dQTrib > 0 then
     Result := Result + #13#10 + FormatQuantidade( dQTrib );
end;

function TACBrBPeDABPEClass.ManterUnidades(sUCom, sUTrib: String): String;
begin
   Result := Trim( sUCom );
   if Trim( sUTrib ) <> '' then
     Result := Result + #13#10 + Trim( sUTrib );
end;

function TACBrBPeDABPEClass.ManterValoresUnitarios(dVCom,
  dVTrib: Double): String;
begin
   Result := FormatValorUnitario ( dVCom );
   if dVTrib > 0 then
     Result := Result + #13#10 + FormatValorUnitario ( dVTrib );
end;

end.
