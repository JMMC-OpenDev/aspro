package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author bourgesl
 */
public class StarObservability {

  private String name;

  private final List<DateTimeInterval> visible = new ArrayList<DateTimeInterval>();

  public StarObservability(final String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public List<DateTimeInterval> getVisible() {
    return visible;
  }

  @Override
  public String toString() {
    return name + " : " + getVisible();
  }
}
