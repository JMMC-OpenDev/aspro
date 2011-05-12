/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import java.util.List;
import javax.swing.AbstractListModel;
import javax.swing.ComboBoxModel;

/**
 * This custom implementation uses an existing List
 *
 * @param <K> type of elements in the List<K>
 *
 * @author bourgesl
 */
public final class GenericListModel<K> extends AbstractListModel implements ComboBoxModel {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /* members */
  /** internal model implementing the List interface */
  private final List<K> model;
  /** flag to enable combo box model implementation (selected item) */
  private final boolean comboBoxModel;
  /** ComboBoxModel selected item */
  private Object selectedObject = null;

  /**
   * Constructor using an existing list for the internal model
   * @param model list to use (must be not null)
   */
  public GenericListModel(final List<K> model) {
    this(model, false);
  }

  /**
   * Constructor using an existing list for the internal model
   * @param model list to use (must be not null)
   * @param isComboBoxModel flag to indicate that this implements ComboBoxModel (selected item)
   */
  public GenericListModel(final List<K> model, final boolean isComboBoxModel) {
    if (model == null) {
      throw new IllegalArgumentException("the given list can not be null !");
    }
    this.model = model;
    this.comboBoxModel = isComboBoxModel;

    // No default selection for combo box model
  }

  /**
   * Returns the component at the specified index.
   * @param      index   an index into this list
   * @return     the component at the specified index
   * @exception  ArrayIndexOutOfBoundsException  if the <code>index</code>
   *             is negative or greater than the current size of this
   *             list
   * @see #get(int)
   */
  public K getElementAt(final int index) {
    return get(index);
  }

  /**
   * Returns the element at the specified position in this list.
   * <p>
   * Throws an <code>ArrayIndexOutOfBoundsException</code>
   * if the index is out of range
   * (<code>index &lt; 0 || index &gt;= size()</code>).
   *
   * @param index index of element to return
   * @return element at the specified position
   * @see List#get(int)
   */
  public K get(final int index) {
    return model.get(index);
  }

  /**
   * Returns the number of components in this list.
   *
   * @return  the number of components in this list
   * @see #size()
   */
  public int getSize() {
    return size();
  }

  /**
   * Returns the number of components in this list.
   *
   * @return  the number of components in this list
   * @see List#size()
   */
  public int size() {
    return model.size();
  }

  /**
   * Tests whether this list has any components.
   *
   * @return  <code>true</code> if and only if this list has
   *          no components, that is, its size is zero;
   *          <code>false</code> otherwise
   * @see List#isEmpty()
   */
  public boolean isEmpty() {
    return model.isEmpty();
  }

  /**
   * Returns a string that displays and identifies this
   * object's properties.
   *
   * @return a String representation of this object
   */
  @Override
  public String toString() {
    return model.toString();
  }

  /**
   * Tests whether the specified object is a component in this list.
   *
   * @param   element   an object
   * @return  <code>true</code> if the specified object
   *          is the same as a component in this list
   * @see List#contains(Object)
   */
  public boolean contains(final K element) {
    return model.contains(element);
  }

  /**
   * Returns the index of the first occurrence of the specified element
   * in this list, or -1 if this list does not contain the element.
   * More formally, returns the lowest index <tt>i</tt> such that
   * <tt>(o==null&nbsp;?&nbsp;get(i)==null&nbsp;:&nbsp;o.equals(get(i)))</tt>,
   * or -1 if there is no such index.
   *
   * @param element element to search for
   * @return the index of the first occurrence of the specified element in
   *         this list, or -1 if this list does not contain the element
   */
  public int indexOf(final K element) {
    return model.indexOf(element);
  }

  /**
   * Replaces the element at the specified position in this list with the
   * specified element.
   * <p>
   * Throws an <code>ArrayIndexOutOfBoundsException</code>
   * if the index is out of range
   * (<code>index &lt; 0 || index &gt;= size()</code>).
   *
   * @param index index of element to replace
   * @param element element to be stored at the specified position
   * @return the element previously at the specified position
   * @see List#set(int, Object)
   */
  public K set(final int index, final K element) {
    final K rv = model.set(index, element);
    fireContentsChanged(this, index, index);
    return rv;
  }

  /**
   * Adds the specified component to the end of this list.
   *
   * @param   obj   the component to be added
   * @see List#add(Object)
   */
  public void add(final K obj) {
    final int index = size();
    model.add(obj);
    fireIntervalAdded(this, index, index);
  }

  /**
   * Inserts the specified element at the specified position in this list.
   * <p>
   * Throws an <code>ArrayIndexOutOfBoundsException</code> if the
   * index is out of range
   * (<code>index &lt; 0 || index &gt; size()</code>).
   *
   * @param index index at which the specified element is to be inserted
   * @param element element to be inserted
   * @see List#add(int, Object)
   */
  public void add(final int index, final K element) {
    model.add(index, element);
    fireIntervalAdded(this, index, index);
  }

  /**
   * Removes the specified element in this list.
   * Returns the element that was removed from the list.
   *
   * @param element element to be removed
   * @return the removed element
   * @see List#remove(Object)
   */
  public K remove(final K element) {
    final int index = model.indexOf(element);
    if (index >= 0) {
      return remove(index);
    }
    return null;
  }

  /**
   * Removes the element at the specified position in this list.
   * Returns the element that was removed from the list.
   * <p>
   * Throws an <code>ArrayIndexOutOfBoundsException</code>
   * if the index is out of range
   * (<code>index &lt; 0 || index &gt;= size()</code>).
   *
   * @param index the index of the element to removed
   * @return the removed element
   * @see List#remove(int)
   */
  public K remove(final int index) {
    final K rv = model.remove(index);
    fireIntervalRemoved(this, index, index);
    return rv;
  }

  /**
   * Removes all of the elements from this list.  The list will
   * be empty after this call returns (unless it throws an exception).
   * @see List#clear()
   */
  public void clear() {
    final int index1 = size() - 1;
    model.clear();
    if (index1 >= 0) {
      fireIntervalRemoved(this, 0, index1);
    }
  }

  /**
   * Deletes the components at the specified range of indexes.
   * The removal is inclusive, so specifying a range of (1,5)
   * removes the component at index 1 and the component at index 5,
   * as well as all components in between.
   * <p>
   * Throws an <code>ArrayIndexOutOfBoundsException</code>
   * if the index was invalid.
   * Throws an <code>IllegalArgumentException</code> if
   * <code>fromIndex &gt; toIndex</code>.
   *
   * @param      fromIndex the index of the lower end of the range
   * @param      toIndex   the index of the upper end of the range
   * @see	   #remove(int)
   */
  public void removeRange(final int fromIndex, final int toIndex) {
    if (fromIndex > toIndex) {
      throw new IllegalArgumentException("fromIndex must be <= toIndex");
    }
    for (int i = toIndex; i >= fromIndex; i--) {
      model.remove(i);
    }
    fireIntervalRemoved(this, fromIndex, toIndex);
  }

  // --- specific methods ------------------------------------------------------
  /**
   * Append the element if not present.
   *
   * @param element element to be added to this list, if absent
   * @return <tt>true</tt> if the element was added
   */
  public boolean addIfAbsent(final K element) {
    if (!this.model.contains(element)) {
      return this.model.add(element);
    }
    return false;
  }

  // implements javax.swing.ComboBoxModel
  /**
   * Set the value of the selected item. The selected item may be null.
   * <p>
   * @param anObject The combo box value or null for no selection.
   */
  public void setSelectedItem(final Object anObject) {
    if (this.comboBoxModel) {
      if ((this.selectedObject != null && !this.selectedObject.equals(anObject))
              || this.selectedObject == null && anObject != null) {
        this.selectedObject = anObject;
        fireContentsChanged(this, -1, -1);
      }
    }
  }

  // implements javax.swing.ComboBoxModel
  public Object getSelectedItem() {
    return this.selectedObject;
  }
}
