{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 28/06/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrGAV
******************************************************************************}

{$I ACBr.inc}

unit ACBrGAVClass;

interface
uses
  Classes,
  {$IFNDEF NOGUI}
    {$IF DEFINED(VisualCLX)}
      QForms,
    {$ELSEIF DEFINED(FMX)}
      FMX.Forms,
    {$ELSE}
      Forms,
    {$IfEnd}
  {$ENDIF}
  ACBrDevice, ACBrECF
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

const
cAberturaIntervalo = 300 ;

type

{ Classe generica de Gaveta, nao implementa nenhum modelo especifico, apenas
  declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de Gavetas como por exemplo a classe TACBrGAVMenno }

TACBrGAVClass = class
  private
    procedure SetAtivo(const Value: Boolean);

  protected
    fpDevice  : TACBrDevice ;
    fpECF     : TACBrECF ;
    fpAtivo   : Boolean ;
    fpProximaAbertura : TDateTime ;
    fsStrComando: String;
    fpModeloStr: String;
    fpAberturaIntervalo: Integer;
    fpAberturaAntecipada : TACBrGAVAberturaAntecipada ;

    function GetGavetaAberta: Boolean; virtual ;
    Procedure CalculaProximaAbertura ; virtual ;

  public
    constructor Create(AOwner: TComponent);
    Destructor Destroy  ; override ;

    Property Ativo  : Boolean read fpAtivo write SetAtivo ;
    procedure Ativar ; virtual ;
    procedure Desativar ; virtual ;
    
    Procedure AbreGaveta  ; virtual ;
    Property GavetaAberta : Boolean read GetGavetaAberta ;

    Property ECF : TACBrECF read fpECF write fpECF ;
    Property ModeloStr: String  read fpModeloStr ;
    property StrComando : String  read fsStrComando write fsStrComando ;
    Property AberturaIntervalo : Integer read fpAberturaIntervalo
       write fpAberturaIntervalo ;
    Property AberturaAntecipada : TACBrGAVAberturaAntecipada
       read fpAberturaAntecipada  write fpAberturaAntecipada ;

end ;

implementation
Uses ACBrGAV, ACBrUtil,
     SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, Windows {$ENDIF} ;

{ TACBrGAVClass }

constructor TACBrGAVClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrGAV) then
     raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrGAV'));

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrGAV,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente}

  fpDevice    := (AOwner as TACBrGAV).Device ;
  fpECF       := (AOwner as TACBrGAV).ECF ;
  fpDevice.SetDefaultValues ;

  fpAtivo     := false ;
  fpModeloStr := 'Não Definida' ;
  fpAberturaIntervalo := cAberturaIntervalo ;
  fpProximaAbertura   := 0 ;
  fpAberturaAntecipada:= aaAguardar ; 
end;

destructor TACBrGAVClass.Destroy;
begin
  fpDevice := nil ; { Apenas remove referencia (ponteiros internos) }
  fpECF    := nil ;

  inherited Destroy;
end;

procedure TACBrGAVClass.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrGAVClass.Ativar;
begin
  if fpAtivo then exit ;

  if fpDevice.Porta <> '' then
     fpDevice.Ativar ;
     
  fpAtivo := true ;
end;

procedure TACBrGAVClass.Desativar;
begin
  if not fpAtivo then exit ;

  if fpDevice.Porta <> '' then
     fpDevice.Desativar ;
     
  fpAtivo := false ;
end;

procedure TACBrGAVClass.AbreGaveta;
begin
  if now < fpProximaAbertura then
     case fpAberturaAntecipada of
        aaIgnorar   :
           exit ;

        aaException :
           raise Exception.Create(ACBrStr('Carregando o Capacitor. Aguarde '+
                 FormatFloat('##',SecondSpan(Now,fpProximaAbertura))+' segundos')) ;

        aaAguardar  :
           while now < fpProximaAbertura do begin
              {$IFNDEF NOGUI}
               if fpDevice.ProcessMessages then
                  Application.ProcessMessages ;
              {$ENDIF}
              Sleep( 50 );
           end ;
     end;
end;

function TACBrGAVClass.GetGavetaAberta: Boolean;
begin
  result := false ;
end;

procedure TACBrGAVClass.CalculaProximaAbertura;
begin
  fpProximaAbertura := IncMilliSecond( now , fpAberturaIntervalo );
end;

end.
