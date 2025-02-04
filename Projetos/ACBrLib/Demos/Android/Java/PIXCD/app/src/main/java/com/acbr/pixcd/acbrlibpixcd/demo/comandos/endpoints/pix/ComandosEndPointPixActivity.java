package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.pix;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.GravityCompat;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.databinding.ActivityComandosEndpointPixBinding;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ViewPagerAdapter;
import com.google.android.material.tabs.TabLayoutMediator;

import java.util.HashMap;
import java.util.Map;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosEndPointPixActivity extends AppCompatActivity {

    private ActivityComandosEndpointPixBinding binding;
    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    @SuppressLint("MissingInflatedId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityComandosEndpointPixBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        configTabLayout();

        application = (PIXCDApplication) getApplicationContext();
        ACBrPIXCD = ACBrLibHelper.getInstance("");
    }

    private void configTabLayout(){
        ViewPagerAdapter adapter = new ViewPagerAdapter(this);
        binding.viewPager.setAdapter(adapter);

        adapter.addFragment(new ComandosConsultarPixFragment(), "ConsultarPIX");
        adapter.addFragment(new ComandosConsultarPixRecebidosFragment(), "ConsultarPIXRecebidos");
        adapter.addFragment(new ComandosSolicitarDevolucaoPixFragment(), "SolicitarDevolucaoPIX");
        adapter.addFragment(new ComandosConsultarDevolucaoPixFragment(), "ConsultarDevolucaoPIX");

        binding.viewPager.setOffscreenPageLimit(adapter.getItemCount());

        new TabLayoutMediator(binding.tabs, binding.viewPager, (tab, position) -> {
            tab.setText(adapter.getTitle(position));
        }).attach();

        Map<Integer, Integer> menuPositionMap = new HashMap<>();
        menuPositionMap.put(R.id.menu_consultar_pix, 0);
        menuPositionMap.put(R.id.menu_consultar_pix_recebidos, 1);
        menuPositionMap.put(R.id.menu_solicitar_devolucao_pix, 2);
        menuPositionMap.put(R.id.menu_consultar_devolucao_pix, 3);

        binding.navigationView.setNavigationItemSelectedListener(item -> {
            Integer position = menuPositionMap.getOrDefault(item.getItemId(), 0);

            binding.viewPager.setCurrentItem(position, true);
            binding.drawerLayout.closeDrawer(GravityCompat.START);
            return true;
        });
    }
}