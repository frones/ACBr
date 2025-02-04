package com.acbr.pixcd.acbrlibpixcd.demo.comandos;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;

import androidx.appcompat.app.AppCompatActivity;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cob.ComandosEndPointCobActivity;
import com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cobv.ComandosEndPointCobVActivity;
import com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.pix.ComandosEndPointPixActivity;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosPIXCDActivity extends AppCompatActivity {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_comandos_pixcd);

        Button btnQRCodeEstatico = findViewById(R.id.btnQRCodeEstatico);
        Button btnEndPointPix = findViewById(R.id.btnEndPointPix);
        Button btnEndPointCob = findViewById(R.id.btnEndPointCob);
        Button btnEndPointCobV = findViewById(R.id.btnEndPointCobV);

        btnQRCodeEstatico.setOnClickListener(view -> IrParaTelaQRCodeEstatico());
        btnEndPointPix.setOnClickListener(view -> IrParaTelaEndPointPix());
        btnEndPointCob.setOnClickListener(view -> IrParaTelaEndPointCob());
        btnEndPointCobV.setOnClickListener(view -> IrParaTelaEndPointCobV());

        this.application = (PIXCDApplication) getApplicationContext();
        this.ACBrPIXCD = application.getACBrLibPIXCD();
    }

    private void IrParaTelaQRCodeEstatico(){
        Intent intent = new Intent(this, ComandosQRCodeEstaticoActivity.class);
        startActivity(intent);
    }

    private void IrParaTelaEndPointPix(){
        Intent intent = new Intent(this, ComandosEndPointPixActivity.class);
        startActivity(intent);
    }

    private void IrParaTelaEndPointCob(){
        Intent intent = new Intent(this, ComandosEndPointCobActivity.class);
        startActivity(intent);
    }

    private void IrParaTelaEndPointCobV(){
        Intent intent = new Intent(this, ComandosEndPointCobVActivity.class);
        startActivity(intent);
    }
}